package org.minima.system.network.minidapps.comms;

import java.io.IOException;
import java.net.BindException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;

import org.minima.utils.MinimaLogger;

public class CommsServer implements Runnable{

	CommsManager mCommsManager;
	
	ServerSocket mServerSocket;
	int mPort = -1;
	
	boolean mRunning = false;
	
	public CommsServer(int zPort,CommsManager zCommsManager) {
		mPort = zPort;
		mCommsManager = zCommsManager;
		
	    MinimaLogger.log("CommsServer started on port : "+mPort);
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
			MinimaLogger.log("CommsServer port:"+mPort+" error:"+e.toString());
			e.printStackTrace();
		}
	}
	
	@Override
	public void run() {
		mRunning = true;
		try {
			//Start a server Socket..
			mServerSocket = new ServerSocket(mPort);
			
			//Keep listening..
			while(mRunning) {
				//Listen in for connections
				Socket clientsock = mServerSocket.accept();
				
				//create a new RPC Handler ..
				CommsClient client = new CommsClient(clientsock,mCommsManager);
				
				//Run in a new Thread
				Thread rpcthread = new Thread(client, "Comms Client");
				rpcthread.start();
			}
			
		} catch (BindException e) {
			//Socket shut down..
			MinimaLogger.log("CommsServer : Port "+mPort+" already in use!.. restart required..");
			
		} catch (SocketException e) {
			if(mRunning) {
				//Socket shut down..
				MinimaLogger.log("CommsServer : Socket Shutdown.. "+e);
			}
		} catch (IOException e) {
			MinimaLogger.log("CommsServer : "+e);
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}
