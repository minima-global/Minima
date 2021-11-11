package org.minima.system.network.mininet;

import java.io.IOException;
import java.net.BindException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;

import org.minima.utils.MinimaLogger;
import org.minima.utils.messages.Message;

public class ClientServer implements Runnable{
	
	ServerSocket mServerSocket;
	int mPort;
	
	boolean mRunning = true;
	
	public ClientServer(int zPort) {
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
		
		MinimaLogger.log("Client Server stopped");
	}
	
	public void start() {
		Thread runner = new Thread(this);
		runner.start();
		
		MinimaLogger.log("Client Server started on port : "+mPort);
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
				
				//Create a message
				Message newclient = new Message(ClientManager.CLIENT_NEWCONNECTION);
				newclient.addObject("socket", clientsock);
				newclient.addBoolean("incoming", true);
				
				//Post it to the manager
				ClientManager.getInstance().PostMessage(newclient);
			}
			
		} catch (BindException e) {
			//Socket shut down..
			MinimaLogger.log("Client Server : Port "+mPort+" already in use!.. restart required..");
			
		} catch (SocketException e) {
			if(mRunning) {
				//Socket shut down..
				MinimaLogger.log("Client Server : Socket Shutdown.. "+e);
			}
		} catch (IOException e) {
			MinimaLogger.log(e);
		}
	}
}