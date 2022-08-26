package org.minima.system.network.rpc;

import java.io.IOException;
import java.net.BindException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;

import org.minima.utils.MinimaLogger;

public abstract class HTTPServer implements Runnable{

	ServerSocket mServerSocket;
	int mPort;
	
	boolean mRunning = true;
	
	public HTTPServer(int zPort) {
		this(zPort, true);
	}
	
	public HTTPServer(int zPort, boolean zAutoStart) {
		mPort = zPort;
		
		if(zAutoStart) {
			start();
		}
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
		
		//MinimaLogger.log("HTTP Server stopped @ "+mPort);
	}
	
	public void start() {
		Thread runner = new Thread(this);
		runner.start();
		
		MinimaLogger.log("HTTP Server started on port : "+mPort);
	}
	
	public abstract Runnable getSocketHandler(Socket zSocket);
	
	@Override
	public void run() {
		try {
			//Start a server Socket..
			mServerSocket = new ServerSocket(mPort);
			
			//Keep listening..
			while(mRunning) {
				//Listen in for connections
				Socket clientsock = mServerSocket.accept();
				
				//Get the Handler..
				Runnable handler = getSocketHandler(clientsock);
				
				//Run in a new Thread
				Thread rpcthread = new Thread(handler, "Socket Handler @ "+getPort());
				rpcthread.setDaemon(true);
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