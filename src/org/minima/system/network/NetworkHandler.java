package org.minima.system.network;

import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Random;

import org.minima.Start;
import org.minima.system.Main;
import org.minima.system.input.InputHandler;
import org.minima.system.network.base.MinimaClient;
import org.minima.system.network.base.MinimaServer;
import org.minima.system.network.minidapps.DAPPManager;
import org.minima.system.network.minidapps.websocket.WebSocketManager;
import org.minima.system.network.rpc.RPCServer;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;

public class NetworkHandler extends MessageProcessor {

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
	 * A list of all the requested TxPoW messages.. 
	 * they could be invalid on arrival as in a different  ranch
	 */
	ArrayList<String> mRequestedTxPoW = new ArrayList<>();
	
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
		super("NETWORK");

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
			
			//Small pause..
			Thread.sleep(200);
			
			//Start the RPC server
			mRPCServer = new RPCServer(getRPCPort());
			Thread rpc = new Thread(mRPCServer, "RPC Server");
			rpc.setDaemon(true);
			rpc.start();
			
			//Small pause..
			Thread.sleep(200);
			
			//Start the DAPP Server
			mDAPPManager = new DAPPManager();
			
			//Start the WebSocket Manager
			mWebSocketManager = new WebSocketManager(getWSPort());
			
		}else if(zMessage.isMessageType(NETWORK_SHUTDOWN)) {
			//Stop the server
			try {mServer.stop();}catch(Exception exc) {}
			
			//Stop the RPC server
			try {mRPCServer.stop();}catch(Exception exc) {}
			
			//Stop the RPC server
			try {mDAPPManager.stop();}catch(Exception exc) {
				MinimaLogger.log(exc);
			}
			
			//Stop the WebSocket server
			try {mWebSocketManager.stop();}catch(Exception exc) {}
			
			//Shutdown all the clients
			for(MinimaClient client : mClients) {
				client.shutdown();
			}
			
			//And finish up..
			stopMessageProcessor();
			
		}else if(zMessage.isMessageType(NETWORK_CONNECT)) {
			String host = zMessage.getString("host");
			int port 	= zMessage.getInteger("port");
			
			MinimaLogger.log("Attempting to connect to "+host+":"+port);
			
			//Create a NetClient
			MinimaClient client = new MinimaClient(host, port, this);
			
			//Store with the rest
			PostMessage(new Message(NETWORK_NEWCLIENT).addObject("client", client));
		
			InputHandler.endResponse(zMessage, true, "Attempting to connect to "+host+":"+port);
			
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
				
				//Is this one of the Initial Host/Port BootStrap Server ?
				boolean bootstrapnode = false;
				for(int i=0;i<Start.VALID_BOOTSTRAP_NODES.length;i++) {
					if(host.equals(Start.VALID_BOOTSTRAP_NODES[i]) && port==9001) {
						bootstrapnode = true;
						break;
					}
				}
				if(bootstrapnode) {
					String oldhost = new String(host);
					host = Start.VALID_BOOTSTRAP_NODES[new Random().nextInt(Start.VALID_BOOTSTRAP_NODES.length)];
					MinimaLogger.log("BOOTSTRAP NODE Connection lost.. resetting from "+oldhost+" to "+host);
				}
				
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
	
	/**
	 * When you request a TxPOW it may be invalid as from a different branch..
	 */
	public void addRequestedInitialSyncTxPow(String zTxPoWID) {
		if(!isRequestedInitialTxPow(zTxPoWID)){
			mRequestedTxPoW.add("INIT_"+zTxPoWID);
		}
	}
	
	public boolean isRequestedInitialTxPow(String zTxPoWID) {
		return mRequestedTxPoW.contains("INIT_"+zTxPoWID);
	}
	
	public void addRequestedTxPow(String zTxPoWID) {
		if(!isRequestedTxPow(zTxPoWID)) {
			mRequestedTxPoW.add(zTxPoWID);	
		}
	}
	
	public boolean isRequestedTxPow(String zTxPoWID) {
		return (mRequestedTxPoW.contains(zTxPoWID) || mRequestedTxPoW.contains("INIT_"+zTxPoWID));
	}
	
	public void removeRequestedTxPow(String zTxPoWID) {
		//Remove link..
		mRequestedTxPoW.remove(zTxPoWID);
		//Just in case was an initial..
		mRequestedTxPoW.remove("INIT_"+zTxPoWID);
	}
	
	public void clearAllrequestedTxPow() {
		mRequestedTxPoW.clear();
	}
	
	public int sizeRequestedTxPow() {
		return mRequestedTxPoW.size();
	}
	
	/**
	 * Get all the current net clients..
	 * @return the list
	 */
	public ArrayList<MinimaClient> getNetClients() {
		return mClients;
	}
	
}
