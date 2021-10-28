package org.minima.system.network.p2p;

import org.minima.database.MinimaDB;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;

public class P2PManager extends MessageProcessor {

	/**
	 * A loop message repeated every so often
	 */
	public static final String P2P_LOOP = new String("P2P_LOOP");
	
	//Every 5 mins..
	public long P2P_LOOP_DELAY			= 1000 * 60 * 5;
	
	public P2PManager() {
		super("P2PMANAGER");
		
		//And start the loop timer..
		PostTimerMessage(new TimerMessage(P2P_LOOP_DELAY, P2P_LOOP));
	}
	
	public JSONObject getStatus() {
		JSONObject ret = new JSONObject();
		
		ret.put("p2psomething", "hello!");
		
		return ret;
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		//For Now..
//		MinimaLogger.log(zMessage.toString());
		
		//Process messages..
		if(zMessage.isMessageType(P2PFunctions.P2P_INIT)) {
			
			//Get the P2P DB
			P2PDB p2pdb = MinimaDB.getDB().getP2PDB();
			
			//Initialise..
			//..
			
		}else if(zMessage.isMessageType(P2PFunctions.P2P_SHUTDOWN)) {			
			
			//Write stuff to P2P DB..
			//..
			
			
			
			//I save the DB.. you don't do it..!
			
			//And finish with..
			stopMessageProcessor();
			
		}else if(zMessage.isMessageType(P2PFunctions.P2P_CONNECTED)) {
		
			//Get the details
			String uid 			= zMessage.getString("uid");
			boolean incoming 	= zMessage.getBoolean("incoming");
			
			//DO Stuff..
			//..
			
			//Send a message
			JSONObject hello = new JSONObject();
			hello.put("greet", "Yo!");
			hello.put("somenumber", 1234);
			
			//Can add other JSONObjects.. 
			JSONArray array = new JSONArray();
			array.add(new String("first"));
			array.add(new String("second.."));
			hello.put("array",array);
			
			//And send it..
			P2PFunctions.sendP2PMessage(uid, hello);
			
		}else if(zMessage.isMessageType(P2PFunctions.P2P_DISCONNECTED)) {
		
			//Get the details
			String uid 			= zMessage.getString("uid");
			boolean incoming 	= zMessage.getBoolean("incoming");
			boolean reconnect 	= zMessage.getBoolean("reconnect");
			
			//Do STUFF..
			//..
			
		}else if(zMessage.isMessageType(P2PFunctions.P2P_MESSAGE)) {
			//Get the message..
			JSONObject message = (JSONObject) zMessage.getObject("message");
			
			//DO STUFF
			//..
		
		}else if(zMessage.isMessageType(P2P_LOOP)) {
			
			//Do Stuff..
			//..
			
			
			//And again..
			PostTimerMessage(new TimerMessage(P2P_LOOP_DELAY, P2P_LOOP));
		}
	}
}
