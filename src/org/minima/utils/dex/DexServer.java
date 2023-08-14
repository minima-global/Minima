package org.minima.utils.dex;

import java.net.Socket;

import org.minima.system.network.rpc.HTTPServer;
import org.minima.utils.MinimaLogger;

public class DexServer {

	static HTTPServer mServer = null;
	
	public static void main(String[] zArgs) {
		
		//Start an HTTP server
		mServer = new HTTPServer(8080) {
			
			@Override
			public Runnable getSocketHandler(Socket zSocket) {
				// TODO Auto-generated method stub
				return new DexManager(zSocket);
			}
		};
		
		//Add a shutdown hook
		Runtime.getRuntime().addShutdownHook(new Thread(){
			@Override
			public void run(){
				mServer.shutdown();
			}
		});
		
	}
}
