package org.minima.system;

import java.io.File;
import java.util.ArrayList;
import java.util.Random;

import org.minima.database.MinimaDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.wallet.ScriptRow;
import org.minima.objects.Pulse;
import org.minima.objects.TxBlock;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.brains.TxPoWMiner;
import org.minima.system.brains.TxPoWProcessor;
import org.minima.system.genesis.GenesisMMR;
import org.minima.system.genesis.GenesisTxPoW;
import org.minima.system.network.NetworkManager;
import org.minima.system.network.maxima.MaximaManager;
import org.minima.system.network.minima.NIOClient;
import org.minima.system.network.minima.NIOManager;
import org.minima.system.network.minima.NIOMessage;
import org.minima.system.network.minima.NIOServer;
import org.minima.system.params.GeneralParams;
import org.minima.system.params.GlobalParams;
import org.minima.utils.MiniFile;
import org.minima.utils.MinimaLogger;
import org.minima.utils.RPCClient;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageListener;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;
import org.minima.utils.messages.TimerProcessor;

public class Main extends MessageProcessor {

	/**
	 * Uptime for the node
	 */
	long mUptimeMilli = System.currentTimeMillis();
	
	/**
	 * Static link to the MAIN class
	 */
	private static Main mMainInstance = null;
	public static Main getInstance() {
		return mMainInstance;
	}
	
	/**
	 * Is there someone listening to Minima messages (Android)
	 */
	private static MessageListener MINIMA_LISTENER = null;
	public static MessageListener getMinimaListener() {
		return MINIMA_LISTENER;
	}
	public static void setMinimaListener(MessageListener zListener) {
		MINIMA_LISTENER = zListener;
	}
	
	/**
	 * Main loop messages
	 */
	public static final String MAIN_TXPOWMINED 	= "MAIN_TXPOWMINED";
	public static final String MAIN_AUTOMINE 	= "MAIN_CHECKAUTOMINE";
	public static final String MAIN_CLEANDB 	= "MAIN_CLEANDB";
	public static final String MAIN_PULSE 		= "MAIN_PULSE";
	
	/**
	 * Network Restart - every 24.5 hours
	 */
	public static final String MAIN_NETRESTART 	= "MAIN_NETRESTART";
//	public static long MAIN_NETRESTART_TIMER 	= (1000 * 60 * 60 * 24) + (1000 * 60 * 30);
	
	/**
	 * Debug Function
	 */
	public static final String MAIN_CHECKER 	= "MAIN_CHECKER";
	MiniData mOldTip 							= MiniData.ZERO_TXPOWID;
	
	/**
	 * Main loop to check various values every 180 seconds..
	 */
	long CHECKER_TIMER							= 1000 * 180;
	
	/**
	 * Notify Users..
	 */
	public static final String MAIN_NEWBLOCK 	= "MAIN_NEWBLOCK";
	public static final String MAIN_BALANCE 	= "MAIN_BALANCE";
	public static final String MAIN_MINING 		= "MAIN_MINING";
	
	/**
	 * Incentive Cash User ping..
	 * 
	 * Every 8 hours
	 */
	public static final String MAIN_INCENTIVE 	= "MAIN_INCENTIVE";
	long IC_TIMER = 1000 * 60 * 60 * 8;
	
	/**
	 * Main TxPoW Processor
	 */
	TxPoWProcessor 	mTxPoWProcessor;
	
	/**
	 * TxPoW Miner
	 */
	TxPoWMiner 		mTxPoWMiner;
	
	/**
	 * Network Manager
	 */
	NetworkManager mNetwork;
	
	/**
	 * Maxima
	 */
	MaximaManager mMaxima;
	
	/**
	 * Are we shutting down..
	 */
	boolean mShuttingdown = false;
	
	/**
	 * Are we restoring..
	 */
	boolean mRestoring = false;
	
	/**
	 * Timer delay for CleanDB messages - every 30 mins
	 */
	long CLEANDB_TIMER	= 1000 * 60 * 30;
	
	/**
	 * Timer for the automine message
	 */
	long AUTOMINE_TIMER = 1000 * 50;
	
	/**
	 * Have all the default keys been created..
	 */
	boolean mInitKeysCreated = false;
	
	public Main() {
		super("MAIN");
	
		//Start the Uptime clock..
		mUptimeMilli = System.currentTimeMillis();
		
		//Reset the static values
		mMainInstance 	= this;
		
		//Create the timer processor
		TimerProcessor.createTimerProcessor();
		
		//Are we deleting previous..
		if(GeneralParams.CLEAN) {
			MinimaLogger.log("Wiping previous config files..");
			//Delete the conf folder
			MiniFile.deleteFileOrFolder(GeneralParams.DATA_FOLDER, new File(GeneralParams.DATA_FOLDER));
		}
		
		//Create the MinmaDB
		MinimaDB.createDB();
		
		//Load the Databases
		MinimaDB.getDB().loadAllDB();
		
		//Set the Base Private seed if needed..
		if(MinimaDB.getDB().getUserDB().getBasePrivateSeed().equals("")) {
			MinimaLogger.log("Generating Base Private Seed Key");
			
			//Not set yet..
			MinimaDB.getDB().getUserDB().setBasePrivateSeed(MiniData.getRandomData(32).to0xString());
		}
		
		//Get the base private seed..
		String basepriv = MinimaDB.getDB().getUserDB().getBasePrivateSeed();
		MinimaDB.getDB().getWallet().initBaseSeed(new MiniData(basepriv));
		
		//Calculate the User hashrate..
		MiniNumber hashrate = TxPoWMiner.calculateHashRate();
		MinimaDB.getDB().getUserDB().setHashRate(hashrate);
		MinimaLogger.log("Calculate device hash rate : "+hashrate.div(MiniNumber.MILLION).setSignificantDigits(4)+" MHs");
		
		//Create the Initial Key Set
		mInitKeysCreated = MinimaDB.getDB().getWallet().initDefaultKeys();
		
		//Start the engine..
		mTxPoWProcessor = new TxPoWProcessor();
		mTxPoWMiner 	= new TxPoWMiner();
		
		//Are we running a private network
		if(GeneralParams.GENESIS) {
			//Create a genesis node
			doGenesis();
		}
		
		//Start the networking..
		mNetwork = new NetworkManager();
		
		//Start up Maxima
		mMaxima = new MaximaManager();
				
		//Simulate traffic message ( only if auto mine is set )
		AUTOMINE_TIMER = MiniNumber.THOUSAND.div(GlobalParams.MINIMA_BLOCK_SPEED).getAsLong();
		PostTimerMessage(new TimerMessage(AUTOMINE_TIMER, MAIN_AUTOMINE));
		
		//Set the PULSE message timer.
		PostTimerMessage(new TimerMessage(GeneralParams.USER_PULSE_FREQ, MAIN_PULSE));
		
		//Clean the DB (delete old records) - 3 minutes after start..
		PostTimerMessage(new TimerMessage(3 * 60 * 1000, MAIN_CLEANDB));
		
		//Store the IC User - do fast first time - 30 seconds in.. then every 8 hours
		PostTimerMessage(new TimerMessage(1000*30, MAIN_INCENTIVE));
		
		//Debug Checker
		PostTimerMessage(new TimerMessage(CHECKER_TIMER, MAIN_CHECKER));
		
		//Quick Clean up..
		System.gc();
	}
	
	/**
	 * Used after a Restore
	 */
	public void setHasShutDown() {
		mShuttingdown = true;
	}
	
	public void shutdown() {
		//Are we already shutting down..
		if(mShuttingdown) {
			return;
		}
		
		//we are shutting down
		mShuttingdown = true;
		
		//No More timer Messages
		TimerProcessor.stopTimerProcessor();
		
		try {
			
			//Tell the wallet - in case we are creating default keys
			MinimaDB.getDB().getWallet().shuttiongDown();
			
			//Shut down the network
			mNetwork.shutdownNetwork();
			
			//Shut down Maxima
			mMaxima.shutdown();
			
			//Stop the Miner
			mTxPoWMiner.stopMessageProcessor();
			
			//Stop the main TxPoW processor
			mTxPoWProcessor.stopMessageProcessor();
			while(!mTxPoWProcessor.isShutdownComplete()) {
				try {Thread.sleep(50);} catch (InterruptedException e) {}
			}
			
			//Wait for the networking to finish
			while(!mNetwork.isShutDownComplete()) {
				try {Thread.sleep(50);} catch (InterruptedException e) {}
			}
			
			//Now backup the  databases
			MinimaDB.getDB().saveAllDB();
					
			//Stop this..
			stopMessageProcessor();
			
			//Wait for it..
			while(!isShutdownComplete()) {
				try {Thread.sleep(50);} catch (InterruptedException e) {}
			}
		
		}catch(Exception exc) {
			MinimaLogger.log("ERROR Shutting down..");
			MinimaLogger.log(exc);
		}
	}
	
	public void restoreReady() {
		//we are about to restore..
		mRestoring = true;
		
		//Shut down the network
		mNetwork.shutdownNetwork();
		
		//Shut down Maxima
		mMaxima.shutdown();
				
		//Stop the Miner
		mTxPoWMiner.stopMessageProcessor();
		
		//Stop the main TxPoW processor
		mTxPoWProcessor.stopMessageProcessor();
		while(!mTxPoWProcessor.isShutdownComplete()) {
			try {Thread.sleep(50);} catch (InterruptedException e) {}
		}
		
		//No More timer Messages
		TimerProcessor.stopTimerProcessor();
		
		//Wait for the networking to finish
		while(!mNetwork.isShutDownComplete()) {
			try {Thread.sleep(50);} catch (InterruptedException e) {}
		}		
	}
	
	public void restartNIO() {
		
		//Not now..
		if(mShuttingdown) {
			return;
		}
		
		//Lock the DB
		MinimaDB.getDB().readLock(true);
		
		try {
			//Log 
			MinimaLogger.log("Network Shutdown started..");
			
			//Shut down the NIO..
			mNetwork.shutdownNetwork();
				
			//Wait for the networking to finish
			while(!mNetwork.isShutDownComplete()) {
				try {Thread.sleep(50);} catch (InterruptedException e) {}
			}
					
			//Wait a second..
			MinimaLogger.log("Network Shutdown complete.. restart in 5 seconds");
			try {Thread.sleep(5000);} catch (InterruptedException e) {}
			
			//Now restart it..
			mNetwork = new NetworkManager();
			
			MinimaLogger.log("Network restarted..");
			
		}catch(Exception exc) {
			
			//Uh oh..
			MinimaLogger.log("[!] Error restarting Network.. Restart Minima!");
			
		}finally {
			
			//UNLock the DB
			MinimaDB.getDB().readLock(false);
		}
	}
	
	public long getUptimeMilli() {
		return System.currentTimeMillis() - mUptimeMilli;
	}
	
	public NetworkManager getNetworkManager() {
		return mNetwork;
	}
	
	public NIOManager getNIOManager() {
		return mNetwork.getNIOManager();
	}
	
	public TxPoWProcessor getTxPoWProcessor() {
		return mTxPoWProcessor;
	}
	
	public TxPoWMiner getTxPoWMiner() {
		return mTxPoWMiner;
	}
	
	public MaximaManager getMaxima() {
		return mMaxima;
	}
	
	public void setTrace(boolean zTrace, String zFilter) {
		setFullLogging(zTrace,zFilter);
		
		mTxPoWProcessor.setFullLogging(zTrace,zFilter);
		mTxPoWMiner.setFullLogging(zTrace,zFilter);
		
		mNetwork.getNIOManager().setFullLogging(zTrace,zFilter);
		mNetwork.getP2PManager().setFullLogging(zTrace,zFilter);
		mNetwork.getSSHManager().setFullLogging(zTrace,zFilter);
		
		NIOClient.mTraceON = zTrace;
		NIOServer.mTraceON = zTrace;
	}
	
	private void doGenesis() {
		
		//Create a new address - to receive the genesis funds..
		ScriptRow scrow = MinimaDB.getDB().getWallet().createNewSimpleAddress(false);
		
		//Create the Genesis TxPoW..
		GenesisTxPoW genesis = new GenesisTxPoW(scrow.getAddress());
		
		//Hard add to the DB
		MinimaDB.getDB().getTxPoWDB().addTxPoW(genesis);
		
		//Create the Genesis TxBlock
		TxBlock txgenesisblock = new TxBlock(new GenesisMMR(), genesis, new ArrayList<>());
		
		//The first root node
		TxPoWTreeNode gensisnode = new TxPoWTreeNode(txgenesisblock);
		
		//Set it
		MinimaDB.getDB().getTxPoWTree().setRoot(gensisnode);
		
		//And set this txpow as main chain..
		MinimaDB.getDB().getTxPoWDB().setOnMainChain(genesis.getTxPoWID());
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		//Are we shutting down
		if(mShuttingdown || mRestoring) {
			return;
		}
		
		//Process messages
		if(zMessage.getMessageType().equals(MAIN_TXPOWMINED)) {
			//Get it..
			TxPoW txpow = (TxPoW) zMessage.getObject("txpow");
			
			//We have mined a TxPoW.. is it atleast a transaction
			if(!txpow.isTransaction() && !txpow.isBlock()) {
				return;
			}
			
			//Did we find a block.. only tell me on the main net.. too easy on Test
			if(!GeneralParams.TEST_PARAMS && txpow.isBlock()) {
				MinimaLogger.log("You found a block! "+txpow.getBlockNumber()+" "+txpow.getTxPoWID());
			}
			
			//Create an NIO Message - so the message goes through the same checks as any other message
			MiniData niodata = NIOManager.createNIOMessage(NIOMessage.MSG_TXPOW, txpow);

			//And send
			Message newniomsg = new Message(NIOManager.NIO_INCOMINGMSG);
			newniomsg.addString("uid", "0x00");
			newniomsg.addObject("data", niodata);

			//Post to the NIOManager - which will check it and forward if correct
			getNetworkManager().getNIOManager().PostMessage(newniomsg);
		
		}else if(zMessage.getMessageType().equals(MAIN_AUTOMINE)) {
			
			//Create a TxPoW
			mTxPoWMiner.PostMessage(TxPoWMiner.TXPOWMINER_MINEPULSE);
			
			//TESTNET - has a small random delay as block speed faster - so no constant overlap
			if(GeneralParams.TEST_PARAMS) {
				//Next Attempt +/- 5 secs, minimum 5 secs
				long minerdelay = AUTOMINE_TIMER + ( 2500L - (long)new Random().nextInt(5000));
				if(minerdelay < 5000) {
					minerdelay = 5000;
				}
				
				//Post the Next AUTOMINE message
				PostTimerMessage(new TimerMessage(minerdelay, MAIN_AUTOMINE));
			
			}else {
				
				//Post the Next AUTOMINE message
				PostTimerMessage(new TimerMessage(AUTOMINE_TIMER, MAIN_AUTOMINE));
			}
			
		}else if(zMessage.getMessageType().equals(MAIN_CLEANDB)) {
			
			//Do some house keeping on the DB
			MinimaDB.getDB().getTxPoWDB().cleanDB();
			
			//Same with the ArchiveDB
			MinimaDB.getDB().getArchive().cleanDB();
			
			//Now save the state - in case system crashed..
			MinimaDB.getDB().saveState();
			
			//Clean up the RAM Memory
			System.gc();
			
			//Do it again..
			PostTimerMessage(new TimerMessage(CLEANDB_TIMER, MAIN_CLEANDB));
		
		}else if(zMessage.getMessageType().equals(MAIN_PULSE)) {
			
			//Create Pulse Message
			Pulse pulse = Pulse.createPulse();
		
			//And send it to all your peers..
			NIOManager.sendNetworkMessageAll(NIOMessage.MSG_PULSE, pulse);
		
//			//Mine a TxPoW
//			mTxPoWMiner.PostMessage(TxPoWMiner.TXPOWMINER_MINEPULSE);
			
			//And then wait again..
			PostTimerMessage(new TimerMessage(GeneralParams.USER_PULSE_FREQ, MAIN_PULSE));
		
		}else if(zMessage.getMessageType().equals(MAIN_INCENTIVE)) {
			
			//Do it agin..
			PostTimerMessage(new TimerMessage(IC_TIMER, MAIN_INCENTIVE));
			
			//Get the User
			String user = MinimaDB.getDB().getUserDB().getIncentiveCashUserID();
			
			//Make sure there is a User specified
			if(!user.equals("")) {
				//Call the RPC End point..
				RPCClient.sendPUT("https://incentivecash.minima.global/api/ping/"+user+"?version="+GlobalParams.MINIMA_VERSION);
			}
			
			
		}else if(zMessage.getMessageType().equals(MAIN_NEWBLOCK)) {
			
			//Get the TxPoW
			TxPoW txpow = (TxPoW) zMessage.getObject("txpow");
			
			//Notify The Web Hook Listeners
			JSONObject data = new JSONObject();
			data.put("txpow", txpow.toJSON());
			
			//And Post it..
			PostNotifyEvent("NEWBLOCK", data);
			
		}else if(zMessage.getMessageType().equals(MAIN_BALANCE)) {
			
			//And Post it..
			PostNotifyEvent("NEWBALANCE", new JSONObject());
				
		}else if(zMessage.getMessageType().equals(MAIN_MINING)) {
			
			//Get the TxPoW
			TxPoW txpow = (TxPoW) zMessage.getObject("txpow");
					
			//Are we starting or stopping..
			boolean starting = zMessage.getBoolean("starting");
			
			//Notify The Web Hook Listeners
			JSONObject data = new JSONObject();
			data.put("txpow", txpow.toJSON());
			data.put("mining", starting);
			
			//And Post it..
			PostNotifyEvent("MINING", data);
			
		}else if(zMessage.getMessageType().equals(MAIN_NETRESTART)) {
			
			//Restart the Networking..
			restartNIO();
			
		}else if(zMessage.getMessageType().equals(MAIN_CHECKER)) {
			
			//Check the Default keys
			if(!mInitKeysCreated) {
				mInitKeysCreated = MinimaDB.getDB().getWallet().initDefaultKeys();
				if(mInitKeysCreated) {
					MinimaLogger.log("All default getaddress keys created..");
				}
			}
			
			//Get the Current Tip
			TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
			if(tip == null) {
				MinimaLogger.log("No tip found in Main Checker..");
				return;
			}
			
			//Has it changed
			if(tip.getTxPoW().getTxPoWIDData().isEqual(mOldTip)) {
				MinimaLogger.log("Warning : Chain tip hasn't changed in 180 seconds "+tip.getTxPoW().getTxPoWID()+" "+tip.getTxPoW().getBlockNumber().toString());
			}
			
			//Keep for the next round
			mOldTip = tip.getTxPoW().getTxPoWIDData();
			
			//A Ping Message.. The top TxPoWID
			NIOManager.sendNetworkMessageAll(NIOMessage.MSG_PING, tip.getTxPoW().getTxPoWIDData());
			
			//Check again..
			PostTimerMessage(new TimerMessage(CHECKER_TIMER, MAIN_CHECKER));
		}
	}
	
	/**
	 * Post a network message to the webhook / Android listeners
	 */
	public void PostNotifyEvent(String zEvent, JSONObject zData) {
		if(getNetworkManager() != null) {
			
			//Create the JSON Message
			JSONObject notify = new JSONObject();
			notify.put("event", zEvent);
			notify.put("data", zData);
			
			//And post
			getNetworkManager().getNotifyManager().PostEvent(notify);
		}
	}
	
}
