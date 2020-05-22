package org.minima.system.network.websocket;

import java.util.ArrayList;

import org.minima.system.Main;
import org.minima.system.SystemHandler;
import org.minima.utils.messages.Message;

public class WebSocketManager extends SystemHandler {

	/**
	 * Message sent from the client..
	 */
	public static final String WEBSOCK_ONOPEN      = "WEBSOCK_ONOPEN";
	public static final String WEBSOCK_ONCLOSE     = "WEBSOCK_ONCLOSE";
	public static final String WEBSOCK_ONMESSAGE   = "WEBSOCK_ONMESSAGE";
	public static final String WEBSOCK_ONEXCEPTION = "WEBSOCK_ONEXCEPTION";
	
	//The BASE WebSocketServer
	WebSocketServer mWebSockServer;
	
	//The List of all the currently connected webpages..
	ArrayList<MinimaWebSocket> mMininaSockets;
	
	/**
	 * Main Constructor
	 * @param zMain
	 * @param zPort
	 */
	public WebSocketManager(Main zMain, int zPort) {
		super(zMain,"WEBSOCKETMANAGER");
		
		mMininaSockets = new ArrayList<>();
		
		//Start a Server
		mWebSockServer = new WebSocketServer(zPort, this);
	}

	public void stop() {
		//Stop the WebSocket Server
		mWebSockServer.stop();
		
		//Stop the message processor..
		stopMessageProcessor();
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		if(zMessage.getMessageType().equals(WEBSOCK_ONOPEN)) {
			//New WS Socket..
			
			
		}
	}
	

	
}
