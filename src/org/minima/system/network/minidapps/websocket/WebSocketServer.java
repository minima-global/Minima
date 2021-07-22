package org.minima.system.network.minidapps.websocket;

import org.minima.system.Main;
import org.minima.utils.MinimaLogger;
import org.minima.utils.nanohttpd.protocols.http.IHTTPSession;
import org.minima.utils.nanohttpd.protocols.websockets.NanoWSD;
import org.minima.utils.nanohttpd.protocols.websockets.WebSocket;

public class WebSocketServer extends NanoWSD {

	WebSocketManager mWSManager;
	
	public WebSocketServer(int zPort, WebSocketManager zWSManager) {
		super(zPort);
		
		//Keep this..
		mWSManager = zWSManager;
		
		//SSL ?
		if(Main.getMainHandler().getNetworkHandler().isSSLEnabled()) {
			makeSecure(Main.getMainHandler().getNetworkHandler().getSSLServerFactory(), null);
		}
				
		//Log it..
		MinimaLogger.log("WebSocket Server started on port "+zPort);
	}

	@Override
	protected WebSocket openWebSocket(IHTTPSession zHTTPSession) {
		return new MinimaWebSocket(zHTTPSession, mWSManager);
	}
}
