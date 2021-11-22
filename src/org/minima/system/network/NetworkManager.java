package org.minima.system.network;

import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.util.Enumeration;

import org.minima.database.MinimaDB;
import org.minima.database.userprefs.UserDB;
import org.minima.system.network.minima.NIOManager;
import org.minima.system.network.p2p.P2PFunctions;
import org.minima.system.network.p2p.P2PManager;
import org.minima.system.network.rpc.RPCServer;
import org.minima.system.network.sshtunnel.SSHManager;
import org.minima.system.network.webhooks.NotifyManager;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;

public class NetworkManager {

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
	RPCServer mRPCServer = null;
	
	/**
	 * The SSH Tunnel Manager
	 */
	SSHManager mSSHManager;
	
	/**
	 * The Web Hooks for Minima messages
	 */
	NotifyManager mNotifyManager;
	
	public NetworkManager() {
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
		mNIOManager = new NIOManager();
		
		//Do we start the RPC server
		if(MinimaDB.getDB().getUserDB().isRPCEnabled()) {
			startRPC();
		}
		
		//Start the SSH Tunnel manager
		mSSHManager = new SSHManager();
		
		//Notifucation of Events
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
		JSONObject sshsettings = udb.getSSHTunnelSettings();
		if(udb.isSSHTunnelEnabled()) {
			stats.put("host", sshsettings.get("host"));
			stats.put("port", sshsettings.get("remoteport"));
			
		}else {
			stats.put("host", GeneralParams.MINIMA_HOST);
			stats.put("port", GeneralParams.MINIMA_PORT);
		}
		
		stats.put("connecting", mNIOManager.getConnnectingClients());
		stats.put("connected", mNIOManager.getConnectedClients());
		
		//RPC Stats
		stats.put("rpc", MinimaDB.getDB().getUserDB().isRPCEnabled());
		
		//P2P stats
		if(GeneralParams.P2P_ENABLED) {
			stats.put("p2p", ((P2PManager)mP2PManager).getStatus(false));
		}else {
			stats.put("p2p", "disabled");
		}
		
		//SSH Tunnel
		JSONObject ssh = new JSONObject();
		if(udb.isSSHTunnelEnabled()) {
			ssh.put("enabled", true);
			ssh.put("user", sshsettings.get("username")+"@"+sshsettings.get("host"));
		}else {
			ssh.put("enabled", false);
		}
		
		stats.put("sshtunnel", ssh);
		
		return stats;
	}
	
	public void startRPC() {
		if(mRPCServer == null) {
			//Start The RPC server
			mRPCServer = new RPCServer(GeneralParams.RPC_PORT);
		}else {
			//Already started..
		}
	}
	
	public void stopRPC() {
		if(mRPCServer != null) {
			//Stop the RPC
			mRPCServer.stop();
			mRPCServer = null;
		}
	}
	
	public void shutdownNetwork() {
		//And the RPC
		stopRPC();
		
		//Stop the NIO Manager
		mNIOManager.PostMessage(NIOManager.NIO_SHUTDOWN);
		
		//Send a message to the P2P
		mP2PManager.PostMessage(P2PFunctions.P2P_SHUTDOWN);
		
		//And the SSH
		mSSHManager.PostMessage(SSHManager.SSHTUNNEL_SHUTDOWN);
		
		//And the notify Manager
		mNotifyManager.shutDown();
	}
	
	public boolean isShutDownComplete() {
		return 		mNIOManager.isShutdownComplete() 
				&& 	mP2PManager.isShutdownComplete();
	}
	
	public MessageProcessor getP2PManager() {
		return mP2PManager;
	}
	
	public NIOManager getNIOManager() {
		return mNIOManager;
	}
	
	public SSHManager getSSHManager() {
		return mSSHManager;
	}
	
	public NotifyManager getNotifyManager() {
		return mNotifyManager;
	}
}
