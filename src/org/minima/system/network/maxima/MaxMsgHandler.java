package org.minima.system.network.maxima;

import org.minima.objects.base.MiniData;
import org.minima.utils.MinimaLogger;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;

public class MaxMsgHandler extends MessageProcessor {

	public static final String MAX_SEND_MESSAGE = "MAX_SEND_MESSAGE";
	
	public MaxMsgHandler() {
		super("MAX_MSG_HANDLER");
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.getMessageType().equals(MAX_SEND_MESSAGE)) {

			//Get the Message 
			Message sendmessage = (Message) zMessage.getObject("msg");
			
			//Who to..
			String host 	= sendmessage.getString("tohost");
			int port		= sendmessage.getInteger("toport");
			
			try {
				//Create the Maxima Message
				MiniData maxmsg = MaximaManager.constructMaximaData(sendmessage);
				
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
					MinimaLogger.log("Warning : Maxima message Unknown Address not delivered to.. "+host+":"+port);
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

}
