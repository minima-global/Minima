package org.minima.system.network.p2p;

import java.util.ArrayList;

import org.minima.system.Main;
import org.minima.system.brains.BackupManager;
import org.minima.system.network.NetworkHandler;
import org.minima.system.network.base.MinimaClient;
import org.minima.utils.JsonDB;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;

public class P2PManager extends MessageProcessor{

	public static final String P2P_INIT = new String("P2P_INIT");
	public static final String P2P_SHUTDOWN = new String("P2P_SHUTDOWN");
	
	public static final String P2P_REPEATLOOP = new String("P2P_REPEATLOOP");
	
	public static final String P2P_RECMESSAGE 	= new String("P2P_RECMESSAGE");
	
	public static final String P2P_SHOWPEERLIST 	= new String("P2P_SHOWPEERLIST");
	
	
	JsonDB mP2PStore;
	
	public P2PManager() {
		super("P2PManager");
	
		//Start the Ball rolling..
		PostMessage(P2P_INIT);
		
		//You can post delayed Messages with 
	}
	
	public void stop() {
		PostMessage(P2P_SHUTDOWN);
	}
	
	protected void saveDB() {
		mP2PStore.saveDB(Main.getMainHandler().getBackupManager().getBackUpFile("p2pdata.json"));
	}
	
	/**
	 * You can use this to get your HOST/IP etc
	 * @return
	 */
	protected NetworkHandler getNetworkHandler(){
		return Main.getMainHandler().getNetworkHandler();
	}
	
	protected ArrayList<MinimaClient> getCurrentMinimaClients(){
		return getNetworkHandler().getNetClients();
	}
	
	protected void sendMessage(MinimaClient zClient, JSONObject zJson) {
		
	}
	
	protected void sendMessageAll(JSONObject zJson) {
		
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.getMessageType().equals(P2P_INIT)) {
			//Get the BackupManager
			BackupManager backup = Main.getMainHandler().getBackupManager();
			
			//Load the DB
			mP2PStore.loadDB(backup.getBackUpFile("p2pdata.json"));
	
			//Start the P2P Checker Loop - every minute
			PostTimerMessage(new TimerMessage(60000, P2P_REPEATLOOP));
			
			//DO stuff.
			//..
			
		}else if(zMessage.getMessageType().equals(P2P_SHUTDOWN)) {
			
			//Do stuff..
			//..
			
			
			//Save any data
			saveDB();
			
			//And stop this Message Processor stack
			stopMessageProcessor();
		
		}else if(zMessage.getMessageType().equals(P2P_REPEATLOOP)) {
			//DO Stuff every minute to check peers etc
			//..
			
			
			//Do it again..
			PostTimerMessage(new TimerMessage(60000, P2P_REPEATLOOP));
		
		}else if(zMessage.getMessageType().equals(P2P_RECMESSAGE)) {
			//Recieved a message over the network..
//			JSON
			
		
		}else if(zMessage.getMessageType().equals(P2P_SHOWPEERLIST)) {
			//Return info to the peerlist function.. from terminal command
			//You can use the 'network function to see a list of current peers
			//but this you can use to test/debug' - use 'peers'
			
			
			
			
		}		
	}

}
