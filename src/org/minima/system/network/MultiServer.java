package org.minima.system.network;

import java.io.IOException;
import java.net.BindException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;

import org.minima.utils.MinimaLogger;
import org.minima.utils.messages.Message;

public class MultiServer implements Runnable{

	NetworkHandler mNetwork;
	
	ServerSocket mServerSocket;
	int mPort;
	String mHost ="127.0.0.1";
	
	//Have we specified our host..
	boolean mHardHostSet = false; 
	String mHardHost = "127.0.0.1";
	
	boolean mRunning = true;
	
	public MultiServer(NetworkHandler zNetwork, int zPort) {
		mNetwork = zNetwork;
		mPort = zPort;
	}
	
	public int getPort() {
		return mPort;
	}
	
	public String getHost() {
		if(mHardHostSet) {
			return mHardHost;
		}
		
		return mHost;
	}
	
	public void hardSetHost(String zHost) {
		mHardHostSet = true;
		mHardHost    = zHost;
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
			mHost = mServerSocket.getInetAddress().getHostAddress();
			
			//Keep listening..
			while(mRunning) {
				//Listen in for connections
				Socket clientsock = mServerSocket.accept();
				
				//create a new Client..
				NetClient client = new NetClient(clientsock, mNetwork);
				
				//Tell the network Handler
				mNetwork.PostMessage(new Message(NetworkHandler.NETWORK_NEWCLIENT).addObject("client", client));
			}
			
		} catch (BindException e) {
			//Socket shut down..
			MinimaLogger.log("Port "+mPort+" allready in use!.. restart required..");
			
		} catch (SocketException e) {
			if(!mRunning) {
				//Socket shut down..
				MinimaLogger.log("Socket Shutdown..");
			}else {
				e.printStackTrace();
			}
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}
