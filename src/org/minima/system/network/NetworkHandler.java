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
import org.minima.system.network.websocket.WebSocketManager;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
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
	
	public static final String NETWORK_WS_NOTIFY 		= "NETWORK_NOTIFY";
	
	/**
	 * The  server listening for clients..
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
	 * HARD SET THE HOST
	 */
	String mHost = "";
	
	/**
	 * 
	 * @param zMain
	 */
	public NetworkHandler(Main zMain, String zHost) {
		super(zMain,"NETWORK");
		
		//Hard set the Host
		mHost = zHost;
		
		//Send a reconnect message every 1/2 hour..
		
	}
	
	public MinimaServer getServer() {
		return mServer;
	}
	
	public RPCServer getRPCServer() {
		return mRPCServer;
	}
	
	public DAPPManager getDAPPManager() {
		return mDAPPManager;
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
			mServer = new MinimaServer(this,port);
			Thread multimain = new Thread(mServer, "Multi Server");
			multimain.setDaemon(true);
			multimain.start();
			
			//Start the RPC server
			mRPCServer = new RPCServer(getMainHandler().getInputHandler(), rpcport);
			Thread rpc = new Thread(mRPCServer, "RPC Server");
			rpc.setDaemon(true);
			rpc.start();
			
			//Start the DAPP Server
			mDAPPManager = new DAPPManager(getMainHandler(), mHost, 21000, rpcport);
			
			//Start the WebSocket Manager
			mWebSocketManager = new WebSocketManager(getMainHandler(), 20999);
			
			//Log it..
			MinimaLogger.log("MiFi proxy set : "+mMifiProxy);
			
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
			for(NetClient client : mClients) {
				client.stopMessageProcessor();
			}
			
			//And finish up..
			stopMessageProcessor();

		}else if(zMessage.isMessageType(NETWORK_WS_NOTIFY)) {
			//System.out.println(zMessage);
			
			//What is the message..
			String json = zMessage.getString("message");
					
			//Notify users that something has changed,,.
			Message msg = new Message(WebSocketManager.WEBSOCK_SENDTOALL);
			msg.addString("message", json);
			mWebSocketManager.PostMessage(msg);
			
		}else if(zMessage.isMessageType(NETWORK_WEBPROXY)) {
			//Connect to a web proxy and listen for RPC calls..
			String uuid 	= zMessage.getString("uuid");
			
			//Create the IP
 			String ip = uuid+"#"+getDAPPManager().getHostIP()+":"+getRPCServer().getPort();
			
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
		
		}else if(zMessage.isMessageType(NETWORK_PING)) {
			
			
		}else if(zMessage.isMessageType(NETWORK_RECONNECT)) {
			//Disconnect and reconnect
			JSONArray shut = new JSONArray();
			for(NetClient client : mClients) {
				//get the UID
				shut.add(client.getUID());
				
				//tell it to shut down..
				client.PostMessage(NetClient.NETCLIENT_SHUTDOWN);
			}
			
			InputHandler.getResponseJSON(zMessage).put("total", shut.size());
			InputHandler.getResponseJSON(zMessage).put("clients", shut);
			InputHandler.endResponse(zMessage, true, "All network clients reset - reconnect in 30 seconds");
			
		}else if(zMessage.isMessageType(NETWORK_DISCONNECT)) {
			String uid = zMessage.getString("uid");
			
			for(NetClient client : mClients) {
				if(client.getUID().equals(uid)) {
					//Don;t want to reconnect if we choose to disconnect
					client.noReconnect();
					
					//tell it to shut down..
					client.PostMessage(NetClient.NETCLIENT_SHUTDOWN);
			
					InputHandler.endResponse(zMessage, true, "Client "+uid+" disconnected - won't reconnect");
					
					return;
				}
			}
			
			InputHandler.endResponse(zMessage, false, "Could not find client UID "+uid);
			
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
			
			//Send to all the clients..
			for(NetClient client : mClients) {
				client.PostMessage(msg);
			}
		}
	}
	
	public ArrayList<NetClient> getNetClients() {
		return mClients;
	}
	
}
