package org.minima.system.network.maxima;

import org.minima.objects.base.MiniData;
import org.minima.utils.MinimaLogger;
import org.minima.utils.messages.Message;


/**
 * Construct (PoW) a maxima message and send
 * 
 * @author spartacusrex
 *
 */
public class MaxSender implements Runnable {

	Message mMessage;
	
	public MaxSender(Message zMaximaMessage) {
		mMessage = zMaximaMessage;
	}
	
	public void send() {
		Thread tt = new Thread(this);
		tt.setDaemon(true);
		tt.start();
	}

	@Override
	public void run() {
		
		//Who to..
		String host 	= mMessage.getString("tohost");
		int port		= mMessage.getInteger("toport");
		
		try {
			//Create the Maxima Message
			MiniData maxmsg = MaximaManager.constructMaximaData(mMessage);
			
			//Send the message
			MiniData validresp = MaximaManager.sendMaxPacket(host,port, maxmsg);
			
			//Check the Response..
			if(validresp.isEqual(MaximaManager.MAXIMA_RESPONSE_OK)) {
				//All fine.. 
			}else if(validresp.isEqual(MaximaManager.MAXIMA_RESPONSE_FAIL)) {
				MinimaLogger.log("Warning : Maxima message not delivered to.. "+host+":"+port);
			}else if(validresp.isEqual(MaximaManager.MAXIMA_RESPONSE_TOOBIG)) {
				MinimaLogger.log("Warning : Maxima message too big not delivered to.. "+host+":"+port);
			}else if(validresp.isEqual(MaximaManager.MAXIMA_RESPONSE_UNKNOWN)) {
				MinimaLogger.log("Warning : Maxima message Unkown Address not delivered to.. "+host+":"+port);
			}else if(validresp.isEqual(MaximaManager.MAXIMA_RESPONSE_WRONGHASH)) {
				MinimaLogger.log("Warning : Maxima message TxPoW Hash wrong not delivered to.. "+host+":"+port);
			}else {
				MinimaLogger.log("Unknown Maxima response message "+validresp.to0xString());
			}
		
		} catch (Exception e) {
			MinimaLogger.log(host+":"+port+" "+e.toString());
		}
	}
}
