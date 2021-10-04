package org.minima.system.network.base;

import java.io.IOException;
import java.net.BindException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;

import org.minima.system.network.NetworkHandler;
import org.minima.utils.MinimaLogger;
import org.minima.utils.messages.Message;

public class MinimaServer implements Runnable{

	NetworkHandler mNetwork;
	
	ServerSocket mServerSocket;
	int mPort;
	
	boolean mRunning = true;
	
	public MinimaServer(NetworkHandler zNetwork, int zPort) {
		mNetwork = zNetwork;
		mPort = zPort;
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
	}
	
	@Override
	public void run() {
		try {
			//Start a server Socket..
			mServerSocket = new ServerSocket(mPort);
			
			MinimaLogger.log("Minima server started on "+mPort);
			
			//Keep listening..
			while(mRunning) {
				//Listen in for connections
				Socket clientsock = mServerSocket.accept();
				
				//create a new Client..
				MinimaClient client = new MinimaClient(clientsock, mNetwork);
				
				//Tell the network Handler
				mNetwork.PostMessage(new Message(NetworkHandler.NETWORK_NEWCLIENT).addObject("client", client).addBoolean("isIncoming", true));
			}
			
		} catch (BindException e) {
			//Socket shut down..
			MinimaLogger.log("Port "+mPort+" allready in use!.. restart required..");
			
		} catch (SocketException e) {
			if(mRunning) {
				MinimaLogger.log("Socket Exception");
				e.printStackTrace();
			}
		} catch (Exception e) {
			// TODO Auto-generated catch block
			MinimaLogger.log("Server Exception");
			e.printStackTrace();
		}
	}
}
