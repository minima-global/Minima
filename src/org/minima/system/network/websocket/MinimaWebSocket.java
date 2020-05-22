package org.minima.system.network.websocket;

import java.io.IOException;

import org.minima.utils.messages.Message;
import org.minima.utils.nanohttpd.protocols.http.IHTTPSession;
import org.minima.utils.nanohttpd.protocols.websockets.CloseCode;
import org.minima.utils.nanohttpd.protocols.websockets.WebSocket;
import org.minima.utils.nanohttpd.protocols.websockets.WebSocketFrame;

public class MinimaWebSocket extends WebSocket {

	WebSocketManager mManager;
	
	public MinimaWebSocket(IHTTPSession zHTTPSession, WebSocketManager zManager) {
		super(zHTTPSession);
		
		//When connection closed can notify
		mManager = zManager;
	}
	
	@Override
	protected void onOpen() {
		//Tell the manager
		Message newclient = new Message(WebSocketManager.WEBSOCK_ONOPEN);
		newclient.addObject("wsclient", this);
		mManager.PostMessage(newclient);	
	}

	@Override
	protected void onClose(CloseCode code, String reason, boolean initiatedByRemote) {
		
	}

	@Override
	protected void onMessage(WebSocketFrame message) {
		
	}

	@Override
	protected void onPong(WebSocketFrame pong) {
		
	}

	@Override
	protected void onException(IOException exception) {
		
	}

}
