package org.minima.system.network.p2p;

import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;

public class P2PPeersChecker extends MessageProcessor {

	/**
	 * Initialise the System
	 */
	public static final String PEERS_INIT 		= new String("PEERS_INIT");
	
	/**
	 * Add some Peers - or more funcxtion..
	 */
	public static final String PEERS_ADDPEERS 	= new String("PEERS_ADDPEERS");
	
	/**
	 * Peers looper called every hour..
	 */
	public static final String PEERS_LOOP 		= new String("PEERS_LOOP");
	long PEERS_LOOP_TIMER = 1000 *60 *60;
	
	
	public P2PPeersChecker() {
		super("PEERS_CHECKER");
		
		//FOR NOW - turm full logs on
		setFullLogging(true, "");
		
		//Do some Initialisation..
		PostMessage(PEERS_INIT);
		
		//First one happens after 5 mins..
		PostTimerMessage(new TimerMessage(1000 * 60 * 5, PEERS_LOOP));
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.getMessageType().equals(PEERS_INIT)) {
			
			
		}else if(zMessage.getMessageType().equals(PEERS_ADDPEERS)) {
			
		
		}else if(zMessage.getMessageType().equals(PEERS_LOOP)) {
		
			
			
			//Do it again ..
			PostTimerMessage(new TimerMessage(PEERS_LOOP_TIMER, PEERS_LOOP));
		}
		
	}

}
