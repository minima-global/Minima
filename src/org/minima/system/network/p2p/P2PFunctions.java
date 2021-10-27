package org.minima.system.network.p2p;

import java.io.IOException;

import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.network.minima.NIOManager;
import org.minima.system.network.minima.NIOMessage;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class P2PFunctions {

	/**
	 * Default messages
	 * 
	 * Other messages can be sent of course
	 */
	public static final String P2P_INIT 			= "P2P_INIT";
	public static final String P2P_SHUTDOWN 		= "P2P_SHUTDOWN";
	
	/**
	 * Tells you the UID and if we attempt reconnect
	 */
	public static final String P2P_CONNECTED 		= "P2P_CONNECTED";
	public static final String P2P_DISCONNECTED 	= "P2P_DISCONNECTED";
	
	/**
	 * P2P message sent from a peer
	 */
	public static final String P2P_MESSAGE 			= "P2P_MESSAGE";
	
	/**
	 * Connect to a Host and port
	 */
	public static void connect(String zHost, int zPort) {
		//Connect Message
		Message msg = new Message(NIOManager.NIO_CONNECT);
		msg.addString("host", zHost);
		msg.addInteger("port", zPort);
		
		//Call the NIOManager
		Main.getInstance().getNIOManager().PostMessage(msg);
	}
	
	/**
	 * Disconnect using the UID
	 */
	public static void disconnect(String zUID) {
		//Call the NIOManager
		Main.getInstance().getNIOManager().disconnect(zUID);
	}
	
	/**
	 * Get ALL the current connections.. 
	 * 
	 * status shows connecting or connected..
	 */
	public static JSONArray getAllConnections() {
		return Main.getInstance().getNetworkManager().getNIOManager().getAllConnections();
	}
	
	/**
	 * Send a message to a specific peer
	 * @throws IOException 
	 */
	public static void sendP2PMessage(String zUID, JSONObject zMessage) throws IOException {
		//Convert the message to a streamable..
		MiniString json = new MiniString(zMessage.toString());
		
		//And now forward..
		NIOManager.sendNetworkMessage(zUID, NIOMessage.MSG_P2P, json);
	}
	
	/**
	 * Send ALL peers a message
	 * @throws IOException 
	 */
	public static void sendP2PMessageAll(JSONObject zMessage) throws IOException {
		sendP2PMessage("", zMessage);
	}
}
