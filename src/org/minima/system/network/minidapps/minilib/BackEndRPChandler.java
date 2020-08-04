package org.minima.system.network.minidapps.minilib;

import org.minima.utils.MinimaLogger;
import org.mozilla.javascript.Function;

public class BackEndRPChandler {

	public void handle(String zCommand, Function zCallback) {
		MinimaLogger.log("MinimaJS - "+zCommand+" "+zCallback);
		
	}
	 
}
