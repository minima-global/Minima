package org.minima.system.network.sshtunnel;

import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;

public class tunnel extends MessageProcessor {

	public static String SSHTUNNEL_INIT   = "SSHTUNNEL_INIT";
	public static String SSHTUNNEL_INFO   = "SSHTUNNEL_INFO";
	public static String SSHTUNNEL_PARAMS = "SSHTUNNEL_PARAMS";
	public static String SSHTUNNEL_START  = "SSHTUNNEL_START";
	public static String SSHTUNNEL_STOP   = "SSHTUNNEL_STOP";
	
	public tunnel() {
		super("SSH_TUNNEL");
	
		//Initialise..
		PostMessage(SSHTUNNEL_INIT);
	}

	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.getMessageType().equals(SSHTUNNEL_INIT)) {
			//Load the setting from the database
			
		}else if(zMessage.getMessageType().equals(SSHTUNNEL_INFO)){
			//Print the details
		
		}else if(zMessage.getMessageType().equals(SSHTUNNEL_PARAMS)){
			//Set the SSH Tunnel parameters
			
		}else if(zMessage.getMessageType().equals(SSHTUNNEL_START)){
			//Start up
			
		}else if(zMessage.getMessageType().equals(SSHTUNNEL_STOP)){
			//Stop it..
			
		}
		
	}

}
