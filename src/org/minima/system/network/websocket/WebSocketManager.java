package org.minima.system.network.websocket;

import java.io.IOException;
import java.util.Enumeration;
import java.util.Hashtable;

import org.minima.system.Main;
import org.minima.system.SystemHandler;
import org.minima.utils.MinimaLogger;
import org.minima.utils.messages.Message;
import org.minima.utils.nanohttpd.protocols.websockets.CloseCode;

public class WebSocketManager extends SystemHandler {

	/**
	 * Message sent from the client..
	 */
	public static final String WEBSOCK_ONOPEN      = "WEBSOCK_ONOPEN";
	public static final String WEBSOCK_ONCLOSE     = "WEBSOCK_ONCLOSE";
	public static final String WEBSOCK_ONMESSAGE   = "WEBSOCK_ONMESSAGE";
	public static final String WEBSOCK_ONEXCEPTION = "WEBSOCK_ONEXCEPTION";
	
	public static final String WEBSOCK_SENDTOALL   = "WEBSOCK_SENDTOALL";
	
	//The BASE WebSocketServer
	WebSocketServer mWebSockServer;
	
	//The List of all the currently connected webpages..
	Hashtable<String, MinimaWebSocket> mMininaSockets;
	
	/**
	 * Main Constructor
	 * @param zMain
	 * @param zPort
	 * @throws IOException 
	 */
	public WebSocketManager(Main zMain, int zPort) throws IOException {
		super(zMain,"WEBSOCKETMANAGER");
		
		mMininaSockets = new Hashtable<>();
		
		//Start a Server
		try {
			mWebSockServer = new WebSocketServer(zPort, this);
			mWebSockServer.start(0,true);
		}catch(IOException exc) {
			MinimaLogger.log("WEBSOCKETMANAGER "+exc);
			mWebSockServer = null;
		}
	}

	public void stop() {
		//Close all the clients..
		Enumeration<MinimaWebSocket> clients = mMininaSockets.elements();
		while(clients.hasMoreElements()) {
			MinimaWebSocket client = clients.nextElement();
			try {
				client.close(CloseCode.NormalClosure, "System Shutdown", false);
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		
		//Stop the WebSocket Server
		if(mWebSockServer  != null) {
			mWebSockServer.stop();	
		}
		
		//Stop the message processor..
		stopMessageProcessor();
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		if(zMessage.getMessageType().equals(WEBSOCK_ONOPEN)) {
			//New WS Socket..
			MinimaWebSocket mws = (MinimaWebSocket) zMessage.getObject("wsclient");
			
			//Get the UID
			String UID = mws.getUID();
			
			//Add it to our list
			mMininaSockets.put(UID, mws);
		
		}else if(zMessage.getMessageType().equals(WEBSOCK_ONCLOSE)) {
			//New WS Socket..
			MinimaWebSocket mws = (MinimaWebSocket) zMessage.getObject("wsclient");
			
			//Get the UID
			String UID = mws.getUID();
			
			//Remove from our List
			mMininaSockets.remove(UID);
		
		}else if(zMessage.getMessageType().equals(WEBSOCK_SENDTOALL)) {
			String msg = zMessage.getString("message");
			Enumeration<MinimaWebSocket> clients = mMininaSockets.elements();
			while(clients.hasMoreElements()) {
				MinimaWebSocket client = clients.nextElement();
				client.send(msg);
			}
		}
		
	}
	

	
}
