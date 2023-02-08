package org.minima.system.network.rpc;

public abstract class Server {

	//What Port are we running on
	protected int mPort = 0;
	
	public Server(int zPort) {
		mPort = zPort;
	}
	
	public int getPort() {
		return mPort;
	}
    
	public abstract void shutdown();
}
