package org.minima.utils.dex;

import java.net.Socket;

import org.minima.system.network.rpc.HTTPServer;
import org.minima.utils.MinimaLogger;

public class DexServer {

	static HTTPServer mServer = null;
	
	static DexManager mDexManager = null;
	public static DexManager getDexManager() {
		return mDexManager;
	}
	
	public static void main(String[] zArgs) {
		
		//Create a global DexManager
		mDexManager = new DexManager();
		
		//Start an HTTP server
		mServer = new HTTPServer(8080) {
			
			@Override
			public Runnable getSocketHandler(Socket zSocket) {
				// TODO Auto-generated method stub
				return new DexHandler(zSocket);
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
