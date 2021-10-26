package org.minima.system.network;

import org.minima.system.network.minima.NIOManager;
import org.minima.system.network.p2p.P2PFunctions;
import org.minima.system.network.p2p.P2PManager;
import org.minima.system.params.GeneralParams;
import org.minima.utils.json.JSONObject;

public class NetworkManager {

	/**
	 * NIO Manager
	 */
	NIOManager mNIOManager;
	
	/**
	 * P2P Manager..
	 */
	P2PManager mP2PManager;
	
	/**
	 * The P2P Functions
	 */
	P2PFunctions mP2P;
	
	public NetworkManager() {
		//The main NIO server manager
		mNIOManager = new NIOManager();
		
		//Create the function handler
		mP2P = new P2PFunctions();
		
		//Create the Managewr
		mP2PManager = new P2PManager(mP2P);
		
		//Post an Init message
		mP2PManager.PostMessage(P2PFunctions.P2P_INIT);
	}
	
	public JSONObject getStatus() {
		JSONObject stats = new JSONObject();
		
		stats.put("port", GeneralParams.MINIMA_PORT);
		stats.put("connecting", mNIOManager.getConnnectingClients());
		stats.put("connected", mNIOManager.getConnectedClients());
		
		//RPC Stats
		//..
		
		//P2P stats
		stats.put("p2p", mP2PManager.getStatus());
		
		return stats;
	}
	
	public void shutdownNetwork() {
		mNIOManager.PostMessage(NIOManager.NIO_SHUTDOWN);
		
		//Send a message to the P2P
		mP2PManager.PostMessage(P2PFunctions.P2P_SHUTDOWN);
	}
	
	public boolean isShutDownComplete() {
		return mNIOManager.isShutdownComplete() && mP2PManager.isShutdownComplete();
	}
	
	public P2PManager getP2PManager() {
		return mP2PManager;
	}
	
	public NIOManager getNIOManager() {
		return mNIOManager;
	}
}
