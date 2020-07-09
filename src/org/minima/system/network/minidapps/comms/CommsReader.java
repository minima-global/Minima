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
import org.minima.objects.greet.TxPoWList;
import org.minima.system.backup.SyncPackage;
import org.minima.system.brains.ConsensusHandler;
import org.minima.system.brains.ConsensusNet;
import org.minima.utils.Crypto;
import org.minima.utils.MiniFormat;
import org.minima.utils.MinimaLogger;
import org.minima.utils.ProtocolException;
import org.minima.utils.messages.Message;

public class CommsReader implements Runnable {
	
	/**
	 * Netclient owner
	 */
	CommsClient mCommsClient;
	
	/**
	 * Constructor
	 * 
	 * @param zCommsClient
	 */
	public CommsReader(CommsClient zCommsClient) {
		mCommsClient = zCommsClient;
	}

	@Override
	public void run() {
		try {
			//Create an input stream
			DataInputStream mInput = new DataInputStream(new BufferedInputStream(mCommsClient.getSocket().getInputStream()));
			
			//The Consensus
//			ConsensusHandler consensus = mNetClient.getNetworkHandler().getMainHandler().getConsensusHandler();
			
			while(true) {
				//Read in the MiniString
				MiniString message = MiniString.ReadFromStream(mInput);
				
				//And Post it..
				Message commsmessage = new Message(CommsClient.COMMSCLIENT_MESSAGE);
				commsmessage.addObject("msg", message);
				mCommsClient.PostMessage(commsmessage);
			}
			
		}catch(Exception exc) {
			//General Exception	
			MinimaLogger.log("COMMSCLIENTREADER ERROR.. "+exc);
			exc.printStackTrace();
		}
		
		//Tell the network Handler
//		mNetClient.getNetworkHandler().PostMessage(new Message(NetworkHandler.NETWORK_CLIENTERROR).addObject("client", mNetClient));
	}
}

