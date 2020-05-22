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
		Message msg = new Message(WebSocketManager.WEBSOCK_ONOPEN);
		msg.addObject("wsclient", this);
		mManager.PostMessage(msg);	
	}

	@Override
	protected void onClose(CloseCode code, String reason, boolean initiatedByRemote) {
		//Tell the manager
		Message msg = new Message(WebSocketManager.WEBSOCK_ONCLOSE);
		msg.addObject("wsclient", this);
		mManager.PostMessage(msg);
	}

	@Override
	protected void onMessage(WebSocketFrame message) {
		//Tell the manager
		Message msg = new Message(WebSocketManager.WEBSOCK_ONMESSAGE);
		msg.addObject("wsclient", this);
		msg.addString("message", message.getTextPayload());
		mManager.PostMessage(msg);
	}

	@Override
	protected void onPong(WebSocketFrame pong) {}

	@Override
	protected void onException(IOException exception) {
		//Tell the manager
		Message msg = new Message(WebSocketManager.WEBSOCK_ONMESSAGE);
		msg.addObject("wsclient", this);
		msg.addString("exception", exception.toString());
		mManager.PostMessage(msg);
	}

}
