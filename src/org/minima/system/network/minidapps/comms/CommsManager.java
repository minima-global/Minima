package org.minima.system.network.minidapps.comms;

import java.util.ArrayList;

import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;

public class CommsManager extends MessageProcessor {

	public static final String COMMS_INIT = "COMMS_INIT";
	
	public static final String COMMS_START = "COMMS_STARTSERVER";
	public static final String COMMS_STOP  = "COMMS_STOPSERVER";
	
	ArrayList<CommsServer> mServers;
	
	public CommsManager() {
		super("COMMSMANAGER");
	
		mServers = new ArrayList<>();
		
		PostMessage(COMMS_INIT);
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.getMessageType().equals(COMMS_INIT)) {
		
			
		}else if(zMessage.getMessageType().equals(COMMS_START)) {
		
			
		}else if(zMessage.getMessageType().equals(COMMS_STOP)) {
		
			
		}
	}

}
