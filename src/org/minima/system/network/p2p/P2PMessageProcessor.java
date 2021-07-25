package org.minima.system.network.p2p;

import java.util.ArrayList;

import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.brains.BackupManager;
import org.minima.system.input.InputHandler;
import org.minima.system.input.functions.send;
import org.minima.system.network.NetworkHandler;
import org.minima.system.network.base.MinimaClient;
import org.minima.utils.JsonDB;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;


public class P2PMessageProcessor extends MessageProcessor{

	/**
	 * P2P Functions..
	 */
	public enum P2PFunctions {
		P2P_INIT(0),
		P2P_SHUTDOWN(1),
		P2P_REPEATLOOP(2),
		P2P_RECMESSAGE(3),
		P2P_SHOWPEERLIST(4);

		private final int msgId;
		private P2PFunctions(int msgId) {
			this.msgId = msgId;
		}

		public int getMsgId() {
			return msgId;
		}
	}
	public static final String P2P_INIT 		= new String("P2P_INIT");
	public static final String P2P_SHUTDOWN 	= new String("P2P_SHUTDOWN");
	public static final String P2P_REPEATLOOP 	= new String("P2P_REPEATLOOP");
	public static final String P2P_RECMESSAGE 	= new String("P2P_RECMESSAGE");
	public static final String P2P_PEERINFO 	= new String("P2P_SHOWPEERLIST");
	
	//The data store
	JsonDB mP2PStore;
	
	public P2PMessageProcessor() {
		super("P2PManager");
	
		mP2PStore = new JsonDB();
		
		//Start the Ball rolling..
		PostMessage(P2P_INIT);
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
	
	/**
	 * All the current connections
	 * @return
	 */
	protected ArrayList<MinimaClient> getCurrentMinimaClients(){
		return getNetworkHandler().getNetClients();
	}
	
	protected void sendMessage(MinimaClient zClient, String zMessage) {
		Message sender = new Message(MinimaClient.NETCLIENT_PEERS);
		sender.addObject("peersinfo", new MiniString(zMessage));
		zClient.PostMessage(sender);
	}
	
	protected void sendMessageAll(String zMessage) {
		ArrayList<MinimaClient> allclient = getCurrentMinimaClients();
		for(MinimaClient client : allclient) {
			sendMessage(client, zMessage);
		}
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
			
			//Send a message ?
			//If JSON..
//			JSONObject obj = new JSONObject();
//			obj.put("data", "something");
//			String str = obj.toString();
			
//			//Connect to someone
//			Message connect  = new Message(NetworkHandler.NETWORK_CONNECT).addInteger("port", port).addString("host", host);
//			getNetworkHandler().PostMessage(connect);
			
			//Disconnect
//			Message disconnect  = getResponseMessage(NetworkHandler.NETWORK_DISCONNECT).addString("uid", uid);
//			getNetworkHandler().PostMessage(disconnect);
			
			//Send a string
			sendMessageAll("Hello!");

			//More stuff..
			//..
			
			//Do it again..
			PostTimerMessage(new TimerMessage(60000, P2P_REPEATLOOP));
		
		}else if(zMessage.getMessageType().equals(P2P_RECMESSAGE)) {
			//Received a message over the network..
			MinimaClient client = (MinimaClient) zMessage.getObject("minimaclient");
			MiniString str 		= (MiniString) zMessage.getObject("peersinfo");
			
			//Do something.. if it's a JSON..
			//JSONObject json = (JSONObject) new JSONParser().parse(str.toString());
			
			MinimaLogger.log("REC Peer info Client "+client.toJSON()+" "+str.toString());
		
		}else if(zMessage.getMessageType().equals(P2P_PEERINFO)) {
			//Return info to the peerlist function.. from terminal command
			//You can use the 'network function to see a list of current peers
			//but this you can use to test/debug' - use 'p2pinfo'
			
			//This is generic way to respond to messages from terminal
			JSONObject resp = InputHandler.getResponseJSON(zMessage);
			
			//Add some details.. 
			resp.put("info", "Some info!");
			resp.put("moreinfo", "Some more info!");
			
			//status true!
			InputHandler.endResponse(zMessage, true, "");
		}		
	}

}
