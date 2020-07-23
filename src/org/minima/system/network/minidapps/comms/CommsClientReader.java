package org.minima.system.network.minidapps.comms;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.IOException;
import java.net.SocketException;

import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.objects.greet.Greeting;
import org.minima.objects.greet.HashNumber;
import org.minima.objects.greet.SyncPackage;
import org.minima.objects.greet.TxPoWList;
import org.minima.system.brains.ConsensusHandler;
import org.minima.system.brains.ConsensusNet;
import org.minima.utils.Crypto;
import org.minima.utils.MiniFormat;
import org.minima.utils.MinimaLogger;
import org.minima.utils.ProtocolException;
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

