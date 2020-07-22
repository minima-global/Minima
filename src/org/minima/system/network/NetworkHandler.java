package org.minima.system.network;

import java.io.IOException;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Enumeration;

import org.minima.system.Main;
import org.minima.system.SystemHandler;
import org.minima.system.input.InputHandler;
import org.minima.system.network.minidapps.DAPPManager;
import org.minima.system.network.rpc.RPCClient;
import org.minima.system.network.rpc.RPCServer;
import org.minima.system.network.websocket.WebSocketManager;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.TimerMessage;

public class NetworkHandler extends SystemHandler{

	public static final String NETWORK_STARTUP 		= "NETWORK_START";
	public static final String NETWORK_SHUTDOWN 	= "NETWORK_SHUTDOWN";
	
	public static final String NETWORK_CONNECT 		= "NETWORK_CONNECT";
	public static final String NETWORK_DISCONNECT 	= "NETWORK_DISCONNECT";
	public static final String NETWORK_RECONNECT 	= "NETWORK_RECONNECT";
	
	public static final String NETWORK_NEWCLIENT 	= "NETWORK_NEWCLIENT";
	public static final String NETWORK_CLIENTERROR 	= "NETWORK_CLIENTERROR";
	
	public static final String NETWORK_PING 		= "NETWORK_PING";
	public static final String NETWORK_PONG 		= "NETWORK_PONG";
	public static final String NETWORK_TRACE 		= "NETWORK_TRACE";
	
	public static final String NETWORK_SENDALL 		= "NETWORK_SENDALL";
	
	public static final String NETWORK_WEBPROXY 	= "NETWORK_WEBPROXY";
	
//	public static final String NETWORK_WS_NOTIFY 	= "NETWORK_NOTIFY";
	
	/**
	 * The Main Minima Server
	 */
	MinimaServer mServer;
	
	/**
	 * The RPC server listening for remote commands
	 */
	RPCServer mRPCServer;
	
	/**
	 * DAPP Server
	 */
	DAPPManager mDAPPManager;
	
	/**
	 * WebSocket Manager
	 */
	WebSocketManager mWebSocketManager;
	
	/**
	 * All the network channels..
	 */
	ArrayList<MinimaClient> mClients 	= new ArrayList<>();
	
	/**
	 * Is reconnect enabled or not ?
	 */
	boolean mGlobalReconnect = true;
	
	/**
	 * HARD SET THE HOST
	 */
	boolean mHardSet = false;
	String mHost     = "";
	
	/**
	 * The Main Minima port - all other ports are added to this one..
	 * 
	 * So you can specify just one port per client.
	 */
	int mBasePort;
	
	/**
	 * 
	 * @param zMain
	 */
	public NetworkHandler(Main zMain, String zHost, int zMainPort) {
		super(zMain,"NETWORK");

		if(zHost.equals("")) {
			mHardSet = false;
			calculateHostIP();
		}else {
			mHardSet = true;
			mHost    = zHost;
		}
		
		//The base port all the other ports are derived from
		mBasePort = zMainPort;
	}
	
	public String getBaseHost() {
		return mHost;
	}
	
	public int getBasePort() {
		return mBasePort;
	}
	
	public int getMinimaPort() {
		return mBasePort;
	}
	
	public int getRPCPort() {
		return mBasePort+1;
	}
	
	public int getWSPort() {
		return mBasePort+2;
	}
	
	public int getMiniDAPPServerPort() {
		return mBasePort+3;
	}
	
	public String calculateHostIP() {
		if(mHardSet) {
			return mHost;
		}
		
		mHost = "127.0.0.1";
		try {
			boolean found = false;
		    Enumeration<NetworkInterface> interfaces = NetworkInterface.getNetworkInterfaces();
	        while (!found && interfaces.hasMoreElements()) {
	            NetworkInterface iface = interfaces.nextElement();
	            // filters out 127.0.0.1 and inactive interfaces
	            if (iface.isLoopback() || !iface.isUp())
	                continue;

	            Enumeration<InetAddress> addresses = iface.getInetAddresses();
	            while(!found && addresses.hasMoreElements()) {
	                InetAddress addr = addresses.nextElement();
	                String ip   = addr.getHostAddress();
	                String name = iface.getDisplayName();
	                
	                //Only get the IPv4
	                if(!ip.contains(":")) {
	                	mHost = ip;
	                	
	                	//If you're on WiFi..
	                	if(name.startsWith("wl")) {
	                		found = true;
	                		break;
	                	}
	                }
	            }
	        }
	    } catch (SocketException e) {
	        MinimaLogger.log("getHostIP : "+e);
	    }
		
		return mHost;
	}
	
	public MinimaServer getMinimaServer() {
		return mServer;
	}
	
	public RPCServer getRPCServer() {
		return mRPCServer;
	}
	
	public DAPPManager getDAPPManager() {
		return mDAPPManager;
	}
	
	public WebSocketManager getWebSocketManager() {
		return mWebSocketManager;
	}	
	
	public void setGlobalReconnect(boolean zGlobalReconnect) {
		mGlobalReconnect = zGlobalReconnect;
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.isMessageType(NETWORK_STARTUP)) {
			MinimaLogger.log("Network Startup..");
			
			//Start the network Server
			mServer = new MinimaServer(this,getMinimaPort());
			Thread multimain = new Thread(mServer, "Multi Server");
			multimain.setDaemon(true);
			multimain.start();
			
			//Start the RPC server
			mRPCServer = new RPCServer(getRPCPort());
			Thread rpc = new Thread(mRPCServer, "RPC Server");
			rpc.setDaemon(true);
			rpc.start();
			
			//Start the DAPP Server
			mDAPPManager = new DAPPManager(getMainHandler());
			
			//Start the WebSocket Manager
			mWebSocketManager = new WebSocketManager(getMainHandler(), getWSPort());
			
		}else if(zMessage.isMessageType(NETWORK_SHUTDOWN)) {
			//Stop the server
			try {mServer.stop();}catch(Exception exc) {}
			
			//Stop the RPC server
			try {mRPCServer.stop();}catch(Exception exc) {}
			
			//Stop the RPC server
			try {mDAPPManager.stop();}catch(Exception exc) {}
			
			//Stop the WebSocket server
			try {mWebSocketManager.stop();}catch(Exception exc) {}
			
			//Shutdown all the clients
			for(MinimaClient client : mClients) {
				client.shutdown();
			}
			
			//And finish up..
			stopMessageProcessor();

//		}else if(zMessage.isMessageType(NETWORK_WS_NOTIFY)) {
//			//What is the message..
//			JSONObject json = (JSONObject) zMessage.getObject("message");
//			
//			String minidappid = "";
//			if(zMessage.exists("minidappid")) {
//				minidappid = zMessage.getString("minidappid");
//			}
//			
//			if(minidappid.equals("")) {
//				Message msg = new Message(WebSocketManager.WEBSOCK_SENDTOALL);
//				msg.addString("message", json.toString());
//				mWebSocketManager.PostMessage(msg);				
//			}else {
//				Message msg = new Message(WebSocketManager.WEBSOCK_SEND);
//				msg.addString("message", json.toString());
//				msg.addString("minidappid", minidappid);
//				mWebSocketManager.PostMessage(msg);
//			}
			
		}else if(zMessage.isMessageType(NETWORK_WEBPROXY)) {
//			//Connect to a web proxy and listen for RPC calls..
//			String uuid 	= zMessage.getString("uuid");
//			
//			//Create the IP
// 			String ip = uuid+"#"+getDAPPManager().getHostIP()+":"+getRPCServer().getPort();
//			
//			//Call the Minima Proxy - this should be user definable..#TODO
//			String url = mMifiProxy+URLEncoder.encode(ip, "UTF-8");
//		
//			//Call it..
//			String resp ="";
//			try {
//				resp = RPCClient.sendGET(url);
//			}catch(IOException exc) {
//				//Tell the user
//				InputHandler.getResponseJSON(zMessage).put("url", url);
//				InputHandler.getResponseJSON(zMessage).put("resp", exc);
//				InputHandler.endResponse(zMessage, true,"");
//				return;
//			}
//			
//			//Tell the user
//			InputHandler.getResponseJSON(zMessage).put("url", url);
//			InputHandler.getResponseJSON(zMessage).put("resp", resp);
//			InputHandler.endResponse(zMessage, true,"");
			
		}else if(zMessage.isMessageType(NETWORK_CONNECT)) {
			String host = zMessage.getString("host");
			int port 	= zMessage.getInteger("port");
			
			MinimaLogger.log("Attempting to connect to "+host+":"+port);
			
			//Create a NetClient
			MinimaClient client = new MinimaClient(host, port, this);
			
			//Store with the rest
			PostMessage(new Message(NETWORK_NEWCLIENT).addObject("client", client));
		
		}else if(zMessage.isMessageType(NETWORK_PING)) {
			
			
		}else if(zMessage.isMessageType(NETWORK_RECONNECT)) {
			//Disconnect and reconnect
			JSONArray shut = new JSONArray();
			for(MinimaClient client : mClients) {
				//get the UID
				shut.add(client.getUID());
				
				//tell it to shut down..
				client.PostMessage(MinimaClient.NETCLIENT_SHUTDOWN);
			}
			
			InputHandler.getResponseJSON(zMessage).put("total", shut.size());
			InputHandler.getResponseJSON(zMessage).put("clients", shut);
			InputHandler.endResponse(zMessage, true, "All network clients reset - reconnect in 30 seconds");
			
		}else if(zMessage.isMessageType(NETWORK_DISCONNECT)) {
			String uid = zMessage.getString("uid");
			
			for(MinimaClient client : mClients) {
				if(client.getUID().equals(uid)) {
					//Don;t want to reconnect if we choose to disconnect
					client.noReconnect();
					
					//tell it to shut down..
					client.PostMessage(MinimaClient.NETCLIENT_SHUTDOWN);
			
					InputHandler.endResponse(zMessage, true, "Client "+uid+" disconnected - won't reconnect");
					
					return;
				}
			}
			
			InputHandler.endResponse(zMessage, false, "Could not find client UID "+uid);
			
		}else if(zMessage.isMessageType(NETWORK_NEWCLIENT)) {
			//get the client
			MinimaClient client = (MinimaClient)zMessage.getObject("client");
			
			//Add it
			mClients.add(client);
			
		}else if(zMessage.isMessageType(NETWORK_CLIENTERROR)) {
			//get the client
			MinimaClient client = (MinimaClient)zMessage.getObject("client");
			
			//Is it a reconnect-er ?
			boolean reconnect = client.isReconnect();
			if(reconnect && mGlobalReconnect) {
				String host = client.getHost();
				int port    = client.getPort();
				
				//And post a message..
				TimerMessage  recon = new TimerMessage(30000,NETWORK_CONNECT);
				recon.addString("host", host);
				recon.addInteger("port", port);
				
				MinimaLogger.log("Attempting reconnect to "+host+":"+port+" in 30s..");
				
				PostTimerMessage(recon);
			}
			
			//Remove him from our list..
			mClients.remove(client);
		
			//Shut him down..
			client.PostMessage(new Message(MinimaClient.NETCLIENT_SHUTDOWN));
			
		}else if(zMessage.isMessageType(NETWORK_TRACE)) {
			boolean traceon = zMessage.getBoolean("trace");
			
			setLOG(traceon);
			
			for(MinimaClient client : mClients) {
				client.setLOG(traceon);
			}
		
		}else if(zMessage.isMessageType(NETWORK_SENDALL)) {
			//Get the message to send
			Message msg = (Message)zMessage.getObject("message");
			
			//Send to all the clients..
			for(MinimaClient client : mClients) {
				client.PostMessage(msg);
			}
		}
	}
	
	public ArrayList<MinimaClient> getNetClients() {
		return mClients;
	}
	
}
