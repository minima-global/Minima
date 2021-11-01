package org.minima.system.network;

import org.minima.system.network.minima.NIOManager;
import org.minima.system.network.p2p.P2PFunctions;
import org.minima.system.network.p2p.P2PManager;
import org.minima.system.params.GeneralParams;
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
	
	public NetworkManager() {
		//The main NIO server manager
		mNIOManager = new NIOManager();
		
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
		if(GeneralParams.P2P_ENABLED) {
			stats.put("p2p", ((P2PManager)mP2PManager).getStatus());
		}else {
			stats.put("p2p", "disabled");
		}
		
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
	
	public MessageProcessor getP2PManager() {
		return mP2PManager;
	}
	
	public NIOManager getNIOManager() {
		return mNIOManager;
	}
}
