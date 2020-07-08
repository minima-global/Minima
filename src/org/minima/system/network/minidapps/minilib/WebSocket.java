package org.minima.system.network.minidapps.minilib;

import org.minima.utils.MinimaLogger;

public abstract class WebSocket {

	public WebSocket(String zAddress) {
		MinimaLogger.log(" websocket "+zAddress);;
	}

	public abstract void run();

}
