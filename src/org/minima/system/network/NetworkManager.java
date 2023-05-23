package org.minima.system.network;

import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.Socket;
import java.net.SocketException;
import java.time.Duration;
import java.util.Date;
import java.util.Enumeration;

import javax.net.ssl.SSLSocket;

import org.minima.database.MinimaDB;
import org.minima.database.userprefs.UserDB;
import org.minima.system.network.minima.NIOManager;
import org.minima.system.network.minima.NIOTraffic;
import org.minima.system.network.p2p.P2PFunctions;
import org.minima.system.network.p2p.P2PManager;
import org.minima.system.network.rpc.CMDHandler;
import org.minima.system.network.rpc.HTTPSServer;
import org.minima.system.network.rpc.HTTPServer;
import org.minima.system.network.rpc.Server;
import org.minima.system.network.webhooks.NotifyManager;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MiniFormat;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;

public class NetworkManager {

	/**
	 * Have we shut this down..
	 */
	public boolean mShuttingDown = false;
	
	/**
	 * NIO Manager
	 */
	NIOManager mNIOManager;
	
	/**
	 * P2P Manager..
	 */
	MessageProcessor mP2PManager;
	
	/**
	 * The RPC server
	 */
	Server mRPCServer = null;
	
	/**
	 * The Web Hooks for Minima messages
	 */
	NotifyManager mNotifyManager;
	
	public NetworkManager() {
		
		//Not shutting down
		mShuttingDown = false;
		
		//Calculate the local host
		calculateHostIP();
		
		//Is the P2P Enabled
		if(GeneralParams.P2P_ENABLED) {
			//Create the Manager
			mP2PManager = new P2PManager();
		}else {
			//Create a Dummy listener.. 
			mP2PManager = new MessageProcessor("P2P_DUMMY") {
				@Override
				protected void processMessage(Message zMessage) throws Exception {
					if(zMessage.isMessageType(P2PFunctions.P2P_SHUTDOWN)) {
						stopMessageProcessor();
					}
				}
			};
		}
		
		//The main NIO server manager
		mNIOManager = new NIOManager(this);
		
		//Do we start the RPC server
		if(GeneralParams.RPC_ENABLED) {
			startRPC();
		}
		
		//Notification of Events
		mNotifyManager = new NotifyManager();
	}
	
	public void calculateHostIP() {
		
		//Has it been specified at the command line..?
		if(GeneralParams.IS_HOST_SET) {
			return;
		}
		
		//Cycle through all the network interfaces 
		try {
			//Start easy
			GeneralParams.MINIMA_HOST = "127.0.0.1";
			
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
						// This breaks P2P
						GeneralParams.MINIMA_HOST = ip;
	                	
	                	//If you're on WiFi..this is the one
	                	if(name.startsWith("wl")) {
	                		found = true;
	                		break;
	                	}
	                }
	            }
	        }
	    } catch (SocketException e) {
	        MinimaLogger.log("ERROR calculating host IP : "+e);
	    }
	}
	
	public JSONObject getStatus() {
		JSONObject stats = new JSONObject();
		
		UserDB udb 				= MinimaDB.getDB().getUserDB();
		
		stats.put("host", GeneralParams.MINIMA_HOST);
		stats.put("hostset", GeneralParams.IS_HOST_SET);
		stats.put("port", GeneralParams.MINIMA_PORT);
		
		stats.put("connecting", mNIOManager.getNumberOfConnnectingClients());
		stats.put("connected", mNIOManager.getNumberOfConnectedClients());
		
		//RPC Stats
		JSONObject rpcjson = new JSONObject();
		rpcjson.put("enabled", GeneralParams.RPC_ENABLED);
		rpcjson.put("port", GeneralParams.RPC_PORT);
		stats.put("rpc", rpcjson);
		
		//P2P stats
		if(GeneralParams.P2P_ENABLED) {
			stats.put("p2p", ((P2PManager)mP2PManager).getStatus(false));
		}else {
			stats.put("p2p", "disabled");
		}
		
		//Read / Write stats..
		NIOTraffic traffic = mNIOManager.getTrafficListener();
		
		JSONObject readwrite = new JSONObject();
		
		Duration dur = Duration.ofMillis(System.currentTimeMillis() - traffic.getStartTime());
		long mins 	 = dur.toMinutes(); 
		if(mins==0) {
			mins = 1;
		}
		
		
		Date starter = new Date(traffic.getStartTime());
		readwrite.put("from", starter.toString());
		readwrite.put("totalread", MiniFormat.formatSize(traffic.getTotalRead()));
		readwrite.put("totalwrite", MiniFormat.formatSize(traffic.getTotalWrite()));
		readwrite.put("breakdown",traffic.getBreakdown());
		
		long speedread 	= traffic.getTotalRead() / mins;
		long speedwrite = traffic.getTotalWrite() / mins;
		readwrite.put("read",MiniFormat.formatSize(speedread)+"/min");
		readwrite.put("write",MiniFormat.formatSize(speedwrite)+"/min");
		
		
		stats.put("traffic", readwrite);
		
		
		return stats;
	}
	
	public void startRPC() {
		if(mRPCServer == null) {
			
			//Are we SSL
			if(GeneralParams.RPC_SSL) {
				
				//Start The RPC server
				mRPCServer = new HTTPSServer(GeneralParams.RPC_PORT) {
					
					@Override
					public Runnable getSocketHandler(SSLSocket zSocket) {
						return new CMDHandler(zSocket);
					}
				};
				
			}else {
				
				//Start The RPC server
				mRPCServer = new HTTPServer(GeneralParams.RPC_PORT) {
					
					@Override
					public Runnable getSocketHandler(Socket zSocket) {
						return new CMDHandler(zSocket);
					}
				};				
			}
		}
	}
	
	public void stopRPC() {
		if(mRPCServer != null) {
			//Stop the RPC
			mRPCServer.shutdown();
			mRPCServer = null;
		}
	}
	
	public void shutdownNetwork() {
		//We are trying toi shutdown
		mShuttingDown = true;
		
		//And the RPC
		stopRPC();
		
		//Stop the NIO Manager
		mNIOManager.PostMessage(NIOManager.NIO_SHUTDOWN);
		
		//Send a message to the P2P
		if(GeneralParams.P2P_ENABLED) {
			((P2PManager)mP2PManager).shutdown();
		}else {
			mP2PManager.stopMessageProcessor();
		}
//		mP2PManager.PostMessage(P2PFunctions.P2P_SHUTDOWN);
		
		//And the notify Manager
		mNotifyManager.shutDown();
	}
	
	public boolean isShuttingDowen() {
		return mShuttingDown;
	}
	
	public boolean isShutDownComplete() {
		return 		mNIOManager.isShutdownComplete() 
				&&  mP2PManager.isShutdownComplete()
				&&  mNotifyManager.isShutdownComplete();
	}
	
	public MessageProcessor getP2PManager() {
		return mP2PManager;
	}
	
	public NIOManager getNIOManager() {
		return mNIOManager;
	}
	
	public NotifyManager getNotifyManager() {
		return mNotifyManager;
	}
}
