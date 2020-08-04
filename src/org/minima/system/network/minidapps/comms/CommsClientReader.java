package org.minima.system.network.minidapps.comms;

import java.io.BufferedInputStream;
import java.io.DataInputStream;
import java.io.IOException;

import org.minima.objects.base.MiniString;
import org.minima.utils.messages.Message;

public class CommsClientReader implements Runnable {
	
	/**
	 * Netclient owner
	 */
	CommsClient mCommsClient;
	
	/**
	 * Constructor
	 * 
	 * @param zCommsClient
	 */
	public CommsClientReader(CommsClient zCommsClient) {
		mCommsClient = zCommsClient;
	}

	@Override
	public void run() {
		DataInputStream input = null;
		
		String error = "";
		
		try {
			//Create an input stream
			input = new DataInputStream(new BufferedInputStream(mCommsClient.getSocket().getInputStream()));
			
			while(true) {
				//Read in the MiniString
				MiniString message = MiniString.ReadFromStream(input);
				
				//And Post it..
				Message commsmessage = new Message(CommsClient.COMMSCLIENT_RECMESSAGE);
				commsmessage.addString("message", message.toString());
				mCommsClient.PostMessage(commsmessage);
			}
			
		}catch(Exception exc) {
			//General Exception	
			error = exc.toString();
			
			//MinimaLogger.log("COMMSCLIENTREADER ERROR.. "+exc);
			//exc.printStackTrace();
		
		}finally {
			if(input != null) {
				try {
					input.close();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
		
		//Shut down the client..
		mCommsClient.PostMessage(CommsClient.COMMSCLIENT_SHUTDOWN);
		//MinimaLogger.log("COMMSCLIENT CLOSED");
	}
}

