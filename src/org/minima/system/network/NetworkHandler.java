package org.minima.system.network;

import java.io.IOException;
import java.net.URLEncoder;
import java.util.ArrayList;

import org.minima.system.Main;
import org.minima.system.SystemHandler;
import org.minima.system.input.InputHandler;
import org.minima.system.network.minidapps.DAPPManager;
import org.minima.system.network.rpc.RPCClient;
import org.minima.system.network.rpc.RPCServer;
import org.minima.utils.MinimaLogger;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.TimerMessage;

public class NetworkHandler extends SystemHandler{

	public static final String NETWORK_STARTUP 		= "NETWORK_START";
	public static final String NETWORK_SHUTDOWN 	= "NETWORK_SHUTDOWN";
	
	public static final String NETWORK_CONNECT 		= "NETWORK_CONNECT";
	public static final String NETWORK_DISCONNECT 	= "NETWORK_DISCONNECT";
	
	public static final String NETWORK_NEWCLIENT 	= "NETWORK_NEWCLIENT";
	public static final String NETWORK_CLIENTERROR 	= "NETWORK_CLIENTERROR";
	
	public static final String NETWORK_PING 		= "NETWORK_PING";
	public static final String NETWORK_TRACE 		= "NETWORK_TRACE";
	
	public static final String NETWORK_SENDALL 		= "NETWORK_SENDALL";
	public static final String NETWORK_ALLSTOP 		= "NETWORK_ALLSTOP";
	
	public static final String NETWORK_WEBPROXY 	= "NETWORK_WEBPROXY";
	
	public static final String NETWORK_NOTIFY 		= "NETWORK_NOTIFY";
	
	/**
	 * The  server listening for clients..
	 */
	MultiServer mServer;
	
	/**
	 * The RPC server listening for remote commands
	 */
	RPCServer mRPCServer;
	
	/**
	 * DAPP Server
	 */
	DAPPManager mDAPPManager;
	
	/**
	 * All the network channels..
	 */
	ArrayList<NetClient> mClients 	= new ArrayList<>();
	
	/**
	 * Is reconnect enabled or not ?
	 */
	boolean mGlobalReconnect = true;
	
	/**
	 * Which Host for the Minima Web MiFi Proxy
	 */
	String mMifiProxy = "http://mifi.minima.global:9000/";
	
	/**
	 * 
	 * @param zMain
	 */
	public NetworkHandler(Main zMain) {
		super(zMain,"NETWORK");
	}
	
	public MultiServer getServer() {
		return mServer;
	}
	
	public RPCServer getRPCServer() {
		return mRPCServer;
	}

	public void setProxy(String zProxy) {
		mMifiProxy = zProxy; 
		if(!mMifiProxy.endsWith("/")) {
			mMifiProxy +="/";
		}
	}
	
	public void setGlobalReconnect(boolean zGlobalReconnect) {
		mGlobalReconnect = zGlobalReconnect;
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.isMessageType(NETWORK_STARTUP)) {
			MinimaLogger.log("Network Startup..");
			
			//Get the port
			int port 	= zMessage.getInteger("port");
			int rpcport = zMessage.getInteger("rpcport");
			
			//Start the network Server
			mServer = new MultiServer(this,port);
			Thread multimain = new Thread(mServer);
			multimain.start();
			
			//Start the RPC server
			mRPCServer = new RPCServer(getMainHandler().getInputHandler(), rpcport);
			Thread rpc = new Thread(mRPCServer);
			rpc.start();
			
			//Start the DAPP Server
			mDAPPManager = new DAPPManager(getMainHandler(),21000);
			
			//Log it..
			MinimaLogger.log("MiFi proxy set : "+mMifiProxy);
			
		}else if(zMessage.isMessageType(NETWORK_SHUTDOWN)) {
			//Stop the server
			try {mServer.stop();}catch(Exception exc) {}
			
			//Stop the RPC server
			try {mRPCServer.stop();}catch(Exception exc) {}
			
			//Stop the RPC server
			try {mDAPPManager.stop();}catch(Exception exc) {}
			
			//Shutdown all the clients
			Message msg = new Message(NetClient.NETCLIENT_SHUTDOWN);
			for(NetClient client : mClients) {
				client.stopMessageProcessor();
//				client.PostMessage(msg);
			}
			
			//And finish up..
			stopMessageProcessor();
		
//		}else if(zMessage.isMessageType(NETWORK_AUTOCONNECT)) {
//			//Send a TimedMessage..
//			Message connect  = new Message(NetworkHandler.NETWORK_CONNECT)
//				.addInt("port", 80)
//				.addString("host", "ec2-35-178-239-187.eu-west-2.compute.amazonaws.com");
//			
//			PostMessage(connect);
		
		}else if(zMessage.isMessageType(NETWORK_NOTIFY)) {
			//Notify users that something has changed,,.
			
		}else if(zMessage.isMessageType(NETWORK_WEBPROXY)) {
			//Connect to a web proxy and listen for RPC calls..
			String uuid 	= zMessage.getString("uuid");
			
			//Create the IP
			String ip = uuid+"#"+getRPCServer().getHost()+":"+getRPCServer().getPort();
			
			//Call the Minima Proxy - this should be user definable..#TODO
			String url = mMifiProxy+URLEncoder.encode(ip, "UTF-8");
		
			//Call it..
			String resp ="";
			try {
				resp = RPCClient.sendGET(url);
			}catch(IOException exc) {
				//Tell the user
				InputHandler.getResponseJSON(zMessage).put("url", url);
				InputHandler.getResponseJSON(zMessage).put("resp", exc);
				InputHandler.endResponse(zMessage, true,"");
				return;
			}
			
			//Tell the user
			InputHandler.getResponseJSON(zMessage).put("url", url);
			InputHandler.getResponseJSON(zMessage).put("resp", resp);
			InputHandler.endResponse(zMessage, true,"");
			
		}else if(zMessage.isMessageType(NETWORK_CONNECT)) {
			String host = zMessage.getString("host");
			int port 	= zMessage.getInteger("port");
			
			MinimaLogger.log("Attempting to connect to "+host+":"+port);
			
			//Create a NetClient
			NetClient client = new NetClient(host, port, this);
			
			//Store with the rest
			PostMessage(new Message(NETWORK_NEWCLIENT).addObject("client", client));
		
		}else if(zMessage.isMessageType(NETWORK_DISCONNECT)) {
			String uid = zMessage.getString("uid");
			
			for(NetClient client : mClients) {
				if(client.getUID().equals(uid)) {
					//Don;t want to reconnect if we choose to disconnect
					client.noReconnect();
					
					//tell it to shut down..
					client.PostMessage(NetClient.NETCLIENT_SHUTDOWN);
					
					break;
				}
			}
			
		}else if(zMessage.isMessageType(NETWORK_NEWCLIENT)) {
			//get the client
			NetClient client = (NetClient)zMessage.getObject("client");
			
			//Add it
			mClients.add(client);
			
		}else if(zMessage.isMessageType(NETWORK_CLIENTERROR)) {
			//get the client
			NetClient client = (NetClient)zMessage.getObject("client");
			
			//Is it a reconnect-er ?
			boolean reconnect = client.isReconnect();
			if(reconnect && mGlobalReconnect) {
				String host = client.getHost();
				int port    = client.getPort();
				
				//And post a message..
				TimerMessage  recon = new TimerMessage(30000,NETWORK_CONNECT);
				recon.addString("host", host);
				recon.addInt("port", port);
				
				MinimaLogger.log("Attempting reconnect to "+host+":"+port+" in 30s..");
				
				PostTimerMessage(recon);
			}
			
			//Remove him from our list..
			mClients.remove(client);
		
			//Shut him down..
			client.PostMessage(new Message(NetClient.NETCLIENT_SHUTDOWN));
			
		}else if(zMessage.isMessageType(NETWORK_TRACE)) {
			boolean traceon = zMessage.getBoolean("trace");
			
			setLOG(traceon);
			
			for(NetClient client : mClients) {
				client.setLOG(traceon);
			}
		
		}else if(zMessage.isMessageType(NETWORK_SENDALL)) {
			//Get the message to send
			Message msg = (Message)zMessage.getObject("message");
			
			//Post it to all the clients
			sendToAll(msg);
		}
	}
	
	private void sendToAll(Message zMessage) {
		//Send to all the clients..
		for(NetClient client : mClients) {
			client.PostMessage(zMessage);
		}
	}
	
	public ArrayList<NetClient> getNetClients() {
		return mClients;
	}
	
}
