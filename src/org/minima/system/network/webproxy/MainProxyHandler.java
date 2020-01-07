package org.minima.system.network.webproxy;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Random;
import java.util.concurrent.ConcurrentHashMap;

import org.minima.system.input.InputMessage;
import org.minima.utils.MiniFormat;
import org.minima.utils.ResponseStream;
import org.minima.utils.MinimaLogger;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;

public class MainProxyHandler extends MessageProcessor {

	
	public static final String PROXY_STARTUP       = "PROXY_STARTUP";
	public static final String PROXY_SHUTDOWN      = "PROXY_SHUTDOWN";
	
	public static final String PROXY_RPC_REQUEST   = "PROXY_RPC_REQUEST";  
	
	public static final String PROXY_USER_MESSAGE  = "PROXY_USER_MESSAGE";  
	public static final String PROXY_USER_CLOSE    = "PROXY_USER_CLOSE";  
	
	public static final String PROXY_ADMIN_MESSAGE  = "PROXY_ADMIN_MESSAGE";  
	
	/**
	 * This server talks to the users
	 */
	NIOServerHost mUserHost;
	
	/**
	 * This server talks to the web..
	 */
	ProxyRPCServer mGETProxy;
	
	/**
	 * WebSocketServer only used for notification of a change..
	 */
//	RPCWebSocketServer mWebSocketServer;
	
	/**
	 * Set of response request and the response object that needs to be finished
	 */
	ConcurrentHashMap<String, ResponseStream> mResponses = new ConcurrentHashMap<String, ResponseStream>();
	
	/**
	 * Link betwen the user and the web
	 */
	ConcurrentHashMap<Integer, String> mUserToWeb = new ConcurrentHashMap<Integer, String>();
	ConcurrentHashMap<String, Integer> mWebToUser = new ConcurrentHashMap<String, Integer>();
	
	/**
	 * Random number generator
	 */
	Random mRand = new Random();
	
	public MainProxyHandler() {
		super("proxyhandler");
	
		PostMessage(PROXY_STARTUP);
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		System.out.println(zMessage);
		
		if(zMessage.getMessageType().equals(PROXY_STARTUP)) {
			//The web RPC proxy
			mGETProxy = new ProxyRPCServer(this, 8001);
			
			//The user NIO server
			mUserHost = new NIOServerHost(8000, this);
	    	
			//The websocket listener..
//			mWebSocketServer = new RPCWebSocketServer(8002);
//			mWebSocketServer.start();
			
		}else if(zMessage.getMessageType().equals(PROXY_SHUTDOWN)) {
			
			mUserHost.stop();
			
			mGETProxy.stop();
			
//			mWebSocketServer.stop(1000);
			
			//Stop the parent
			stopMessageProcessor();
			
			//Message received from the website
		}else if(zMessage.getMessageType().equals(PROXY_RPC_REQUEST)) {
			//Details
			String request 		= zMessage.getString("request");
			String webid     	= zMessage.getString("webid");
			ResponseStream resp = (ResponseStream) zMessage.getObject("response");
			
			//Check that there is a user for this webid
			Integer userid = mWebToUser.get(webid);
			if(userid == null) {
				//End the response..
				resp.endStatus(false, "User not found..");
				return;
			}
			
			//Create a random key to link to the response
			String randid = webid+mRand.nextInt();
			
			//Check is not already in there
			while(mResponses.containsKey(randid)) {
				randid = webid+mRand.nextInt();
			}
			
			//Put in the set
			mResponses.put(randid, resp);
			
			//Construct a message
			String message = randid+":"+request;
			
			//And now send the request to the user
			mUserHost.sendToUser(message, userid);
			
			//Message received from the user
		}else if(zMessage.getMessageType().equals(PROXY_ADMIN_MESSAGE)) {
			String msg = zMessage.getString("data");
			
			if(msg.equals("exit")) {
				PostMessage(PROXY_SHUTDOWN);
				
			}else if(msg.equals("channels")) {
				System.out.println("All channels : "+mUserHost.mChannels);
			
			}else if(msg.equals("requests")) {
				System.out.println("Pending requests : "+mResponses);
				
			}else if(msg.equals("users")) {
				System.out.println("All user weblinks : "+mUserToWeb);
			
			}else if(msg.equals("help")) {
				System.out.println("channels requests users help exit");
			}
			
		}else if(zMessage.getMessageType().equals(PROXY_USER_CLOSE)) {
			Integer userid  = new Integer(zMessage.getInteger("userid"));
			
			String webid = mUserToWeb.get(userid);
			mWebToUser.remove(webid);
			mUserToWeb.remove(userid);
			
		}else if(zMessage.getMessageType().equals(PROXY_USER_MESSAGE)) {
			//Get the complete string
			String data 	= zMessage.getString("data");
			Integer userid  = new Integer(zMessage.getInteger("userid"));
			
			int index		= data.indexOf(":");
			
			//Is it the initial message
			if(data.startsWith("init:")) {
				//Link this userid to this webid..
				String webid = data.substring(index+1);
				
				//Store it..
				mUserToWeb.put(userid,webid);
				mWebToUser.put(webid,userid);
			
				//Send a message to the Web page..
//				mWebSocketServer.sendMessage(webid, "connected:"+userid);
				
			}else if(data.startsWith("notify:")) {
				//Notify the website over the websocket..
				String webid = mUserToWeb.get(userid);
				
				//Send it..
//				mWebSocketServer.sendMessage(webid, "Balance has changed");
				
			}else {
				//Now break it down
				String randid   = data.substring(0,index);
				String result   = data.substring(index+1);
				
				//Now get the response object
				ResponseStream resp = mResponses.get(randid);
				
				//Now remove it
				mResponses.remove(randid);
				
				//And now set it.. !
				resp.hardEndStatus(result);
			}
		}
	}

	/**
	 * Start here up..
	 * @param zArgs
	 */
	public static void main(String[] zArgs) {
    	//Get the ports..
		//..
		
		//go..
    	MainProxyHandler PROXY = new MainProxyHandler();
    	
    	//Listen for input
	    InputStreamReader is    = new InputStreamReader(System.in);
	    BufferedReader bis      = new BufferedReader(is);

	    //Loop until finished..
	    while(true){
	        try {
	            //Get a line of input
	            String input = bis.readLine().trim();
	            
	            if(!input.equals("")) {
	            	//post it..
	            	PROXY.PostMessage(new Message(PROXY_ADMIN_MESSAGE).addString("data", input));
	            	
	            	if(input.equals("exit")) {
		            	break;
		            } 
	            }
	            
	        } catch (IOException ex) {
	            MinimaLogger.log(""+ex);
	        }
	    }
	    
	    //Cross the streams..
	    try {
	        bis.close();
	        is.close();
	    } catch (IOException ex) {
	    	MinimaLogger.log(""+ex);
	    }
    	
    }
	
}
