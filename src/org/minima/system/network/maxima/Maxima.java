package org.minima.system.network.maxima;

import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;

public class Maxima extends MessageProcessor {

	public static final String MAXIMA_INIT 		= "MAXIMA_INIT";
	public static final String MAXIMA_SHUTDOWN 	= "MAXIMA_SHUTDOWN";
	
	public static final String MAXIMA_MESSAGE 	= "MAXIMA_MESSAGE";
	
	public Maxima() {
		super("MAXIMA");
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.getMessageType().equals(MAXIMA_MESSAGE)) {
			
			//Received a new message..
			
			
			
		}
		
	}

}
