package org.minima.system.network.mininet;

import java.io.BufferedInputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.Socket;

import org.minima.objects.base.MiniData;
import org.minima.utils.MiniFormat;
import org.minima.utils.MinimaLogger;
import org.minima.utils.messages.Message;

public class ClientListener implements Runnable {

	Socket mSocket;
	
	boolean mIncoming;
	
	boolean mRunning;
	
	String mUID;
	
	DataOutputStream mOutput;
	
	public ClientListener(Socket zSocket, boolean zIncoming) throws IOException {
		mUID		= MiniFormat.createRandomString(8);
		mIncoming 	= zIncoming;
		mSocket 	= zSocket;
		mOutput 	= new DataOutputStream(zSocket.getOutputStream());
	}
	
	public boolean isIncoming() {
		return mIncoming;
	}
	
	public String getUID() {
		return mUID;
	}
	
	public DataOutputStream getOutputStream() {
		return mOutput;
	}
	
	@Override
	public void run() {
		
		try {
		
			//Create an input stream
			DataInputStream dis = new DataInputStream(new BufferedInputStream(mSocket.getInputStream()));
			
			while(mRunning) {
				
				//Read in a MiniData message
				MiniData data = MiniData.ReadFromStream(dis);
				
				//Create a message
				Message incoming = new Message(ClientManager.CLIENT_INCOMINGMSG);
				incoming.addString("uid", mUID);
				incoming.addObject("data", data);
				
				//Post that message as New..
				ClientManager.getInstance().PostMessage(incoming);
			}
			
		}catch(Exception exc) {
			MinimaLogger.log(exc);
		}
		
		//Notify that we have disconnected..
		
		
	}

}
