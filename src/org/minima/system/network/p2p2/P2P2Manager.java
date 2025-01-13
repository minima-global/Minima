package org.minima.system.network.p2p2;

import java.util.ArrayList;
import java.util.Random;

import org.minima.database.MinimaDB;
import org.minima.system.Main;
import org.minima.utils.MinimaLogger;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;

public class P2P2Manager extends MessageProcessor{

	//Initialise the systm
	public static String P2P2_INIT	= "P2P2_INIT";
	
	//Loop check connections etc..
	public static String P2P2_FASTLOOP	= "P2P2_FAST_LOOP";
	public long P2P2_LOOP_TIMER		= 1000 * 60 * 5; // 5 mins
	
	public static String P2P2_SLOWLOOP	= "P2P2_SLOW_LOOP";
	public long P2P2_LOOP_TIMER_SLOW	= 1000 * 60 * 60 * 6; // 6 hours
	
	public int NUMBER_DESIRED_CONNECTIONS = 3;
	
	public P2P2Manager() {
		super("P2P2MANAGER");

		//LOOP Check
		PostTimerMessage(new TimerMessage(P2P2_LOOP_TIMER, P2P2_FASTLOOP));
		PostTimerMessage(new TimerMessage(P2P2_LOOP_TIMER_SLOW, P2P2_SLOWLOOP));
	}
	
	public String getRandomPeerFromList() {
		//Get all the valid P2P addresses..
		ArrayList<String> allpeers = MinimaDB.getDB().getP2P2DB().getAllKnownPeers();
		
		if(allpeers.size() == 0) {
			return "";
		}
		
		//Get a random number..
		int rand = new Random().nextInt(allpeers.size());
		
		//Now pick one at random..
		return allpeers.get(rand);
	}
	
	//PING 3 random hosts and connect to the one with the LEAST connections
	public void checkWhichConnect() {
		
		//
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.getMessageType().equals(P2P2_INIT)) {
			
			MinimaLogger.log("P2P2 Inited.. ");
			
		}else if(zMessage.getMessageType().equals(P2P2_FASTLOOP)) {
			
			//Re-Check
			PostTimerMessage(new TimerMessage(P2P2_LOOP_TIMER, P2P2_FASTLOOP));
			
			//Check network available..
			if(!P2P2Functions.isNetAvailable()) {
				return;
			}
			
			//See how many connections we have..
			int numconn = Main.getInstance().getNetworkManager().getNIOManager().getNumberOfConnectedClients();
			if(numconn < NUMBER_DESIRED_CONNECTIONS) {
				
				//Connect to a new Random peer..
				String peer = getRandomPeerFromList();
				
				//Try and connect to him..
				//P2P2Functions.checkConnect(zHost, zPort)
				
				return;
			}
			
		}else if(zMessage.getMessageType().equals(P2P2_SLOWLOOP)) {
			
			//Cycle one of your OUTGOING connections.. close one and connect
			
			//Connect to a new Host
			
		}
		
	}

}
