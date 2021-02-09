package org.minima.system.network.maxima;

import java.io.IOException;
import java.net.BindException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;

import org.minima.utils.MinimaLogger;

public class MaximaServer implements Runnable{

	ServerSocket mServerSocket;
	int mPort;
	
	boolean mRunning = true;
	
	public MaximaServer(int zPort) {
		mPort = zPort;
		
	    MinimaLogger.log("Maxima Server started on port : "+mPort);
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
			
			//Keep listening..
			while(mRunning) {
				//Listen in for connections
				Socket clientsock = mServerSocket.accept();
				
				//create a new RPC Handler ..
				MaximaHandler max = new MaximaHandler(clientsock);
				
				//Run in a new Thread
				Thread maxthread = new Thread(max, "Maxima Client");
				maxthread.start();
			}
			
		} catch (BindException e) {
			//Socket shut down..
			MinimaLogger.log("MaximaServer : Port "+mPort+" already in use!.. restart required..");
			
		} catch (SocketException e) {
			if(mRunning) {
				//Socket shut down..
				MinimaLogger.log("MaximaServer : Socket Shutdown.. "+e);
			}
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}
