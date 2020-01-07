package org.minima.system.network.webproxy;

import java.io.IOException;
import java.net.BindException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;

import org.minima.system.input.InputHandler;
import org.minima.utils.MinimaLogger;

public class ProxyRPCServer implements Runnable{

	MainProxyHandler mHandler;
	
	ServerSocket mServerSocket;
	int mPort;
	
	boolean mRunning = true;
	
	public ProxyRPCServer(MainProxyHandler zHandler, int zPort) {
		mHandler = zHandler;
		mPort = zPort;
		
		//Run it
    	Thread tt = new Thread(this);
    	tt.start();
	}
	
	public int getPort() {
		return mPort;
	}
		
	public void stop() {
		mRunning = false;
		
		try {
			if(mServerSocket != null) {
				mServerSocket.close();
			}
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	@Override
	public void run() {
		try {
			//Start a server Socket..
			mServerSocket = new ServerSocket(mPort);
			
			MinimaLogger.log("ProxyRPCServer running on port "+mPort);
			
			//Keep listening..
			while(mRunning) {
				//Listen in for connections
				Socket clientsock = mServerSocket.accept();
				
				//create a new RPC Handler ..
				ProxyRPCHandler rpc = new ProxyRPCHandler(clientsock, mHandler);
				
				//Run in a new Thread
				Thread rpcthread = new Thread(rpc);
				rpcthread.start();
			}
			
		} catch (BindException e) {
			//Socket shut down..
			MinimaLogger.log("ProxyRPCServer : Port "+mPort+" allready in use!.. restart required..");
			
		} catch (SocketException e) {
			if(!mRunning) {
				//Socket shut down..
				MinimaLogger.log("ProxyRPCServer : Socket Shutdown..");
			}else {
				e.printStackTrace();
			}
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}
