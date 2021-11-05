package org.minima.system.network.rpc;

import java.io.IOException;
import java.net.BindException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;

import org.minima.utils.MinimaLogger;

public class RPCServer implements Runnable{

	ServerSocket mServerSocket;
	int mPort;
	
	boolean mRunning = true;
	
	public RPCServer(int zPort) {
		mPort = zPort;
		start();
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
			MinimaLogger.log(e);
		}
		
		MinimaLogger.log("RPC Server stopped");
	}
	
	public void start() {
		Thread runner = new Thread(this);
		runner.start();
		
		MinimaLogger.log("RPC Server started on port : "+mPort);
	}
	
	@Override
	public void run() {
		try {
			//Start a server Socket..
			mServerSocket = new ServerSocket(mPort);
			
			//Keep listening..
			while(mRunning) {
				//Listen in for connections
				Socket clientsock = mServerSocket.accept();
				
				//create a new RPC Handler ..
				RPCHandler rpc = new RPCHandler(clientsock);
				
				//Run in a new Thread
				Thread rpcthread = new Thread(rpc, "RPC Client");
				rpcthread.start();
			}
			
		} catch (BindException e) {
			//Socket shut down..
			MinimaLogger.log("RPCServer : Port "+mPort+" already in use!.. restart required..");
			
		} catch (SocketException e) {
			if(mRunning) {
				//Socket shut down..
				MinimaLogger.log("RPCServer : Socket Shutdown.. "+e);
			}
		} catch (IOException e) {
			MinimaLogger.log(e);
		}
	}
}