package org.minima.system.network.minidapps.comms;

import java.io.IOException;
import java.net.BindException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;

import org.minima.utils.MinimaLogger;
import org.minima.utils.messages.Message;

public class CommsServer implements Runnable{

	CommsManager mCommsManager;
	String mMiniDAPPID;
	
	ServerSocket mServerSocket;
	int mPort = -1;
	
	boolean mRunning = false;
	
	public CommsServer(int zPort, String zMiniDAPPID, CommsManager zCommsManager) {
		mPort = zPort;
		mMiniDAPPID = zMiniDAPPID;
		mCommsManager = zCommsManager;
		
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
			
			MinimaLogger.log("CommsServer started on port : "+mPort);
		    
			//Tell the Manager..
			Message newserver = new Message(CommsManager.COMMS_NEWSERVER);
			newserver.addObject("server", this);
			newserver.addString("minidappid", mMiniDAPPID);
			mCommsManager.PostMessage(newserver);
			
			//Keep listening..
			while(mRunning) {
				//Listen in for connections
				Socket clientsock = mServerSocket.accept();
				
				//create a new RPC Handler ..
				CommsClient client = new CommsClient(clientsock, mPort, mMiniDAPPID, mCommsManager);
				
				//Run in a new Thread
				Thread rpcthread = new Thread(client, "Comms Client");
				rpcthread.start();
			}
			
		} catch (Exception e) {
			if(mRunning) {
				//Socket shut down..
				MinimaLogger.log("CommsServer : Socket ERROR .. "+e);
				
				//Tell the Manager..
				Message newserver = new Message(CommsManager.COMMS_SERVERERROR);
				newserver.addObject("server", this);
				newserver.addString("minidappid", mMiniDAPPID);
				newserver.addObject("error", e.toString());
				mCommsManager.PostMessage(newserver);
			}
		}	
		
		//Socket shut down..
		MinimaLogger.log("CommsServer stopped on port "+mPort);
	}
}
