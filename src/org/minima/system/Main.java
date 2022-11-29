package org.minima.system;

import java.io.File;
import java.util.ArrayList;

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
import org.minima.system.commands.Command;
import org.minima.system.genesis.GenesisMMR;
import org.minima.system.genesis.GenesisTxPoW;
import org.minima.system.mds.MDSManager;
import org.minima.system.network.NetworkManager;
import org.minima.system.network.maxima.MaximaManager;
import org.minima.system.network.minima.NIOManager;
import org.minima.system.network.minima.NIOMessage;
import org.minima.system.params.GeneralParams;
import org.minima.system.params.GlobalParams;
import org.minima.system.sendpoll.SendPollManager;
import org.minima.utils.BIP39;
import org.minima.utils.MiniFile;
import org.minima.utils.MinimaLogger;
import org.minima.utils.RPCClient;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageListener;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;
import org.minima.utils.messages.TimerProcessor;
import org.minima.utils.ssl.SSLManager;

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
	public static final String MAIN_PULSE 		= "MAIN_PULSE";

	/**
	 * Clean DB - RamDB every 30 mins.. the TxPoW and Archive every 12 hours
	 */
	public static final String MAIN_CLEANDB_RAM 	= "MAIN_CLEANDB_RAM";
	long CLEANDB_RAM_TIMER	= 1000 * 60 * 30;
	
	public static final String MAIN_CLEANDB_SQL 	= "MAIN_CLEANDB_SQL";
	long CLEANDB_SQL_TIMER	= 1000 * 60 * 60 * 12;
	
	/**
	 * Auto backup every 24 hrs..
	 */
	public static final String MAIN_AUTOBACKUP 	= "MAIN_AUTOBACKUP";
	long AUTOBACKUP_TIMER = 1000 * 60 * 60 * 24;
	
	/**
	 * Aync Shutdown call
	 */
	public static final String MAIN_SHUTDOWN 		= "MAIN_SHUTDOWN";
	
	/**
	 * Network Restart
	 */
	public static final String MAIN_NETRESTART 	= "MAIN_NETRESTART";
	public static final String MAIN_NETRESET 	= "MAIN_NETRESET";
	long NETRESET_TIMER = 1000 * 60 * 60 * 24;
	
	
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
	 * MDS
	 */
	MDSManager mMDS;
	
	/**
	 * Send POll Manager
	 */
	SendPollManager mSendPoll;
	
	/**
	 * Are we shutting down..
	 */
	boolean mShuttingdown = false;
	
	/**
	 * Are we restoring..
	 */
	boolean mRestoring = false;
	
	/**
	 * Timer for the automine message
	 */
	public long AUTOMINE_TIMER = 1000 * 50;
	
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
		
		//Create the SSL Keystore..
		SSLManager.makeKeyFile();
		
		//Set the Base Private seed if needed..
		if(MinimaDB.getDB().getUserDB().getBasePrivateSeed().equals("")) {
			MinimaLogger.log("Generating Base Private Seed Key");
			
			//Get a BIP39 phrase
			String[] words = BIP39.getNewWordList();
			
			//Convert to a string
			String phrase = BIP39.convertWordListToString(words);
			
			//Convert that into a seed..
			MiniData seed = BIP39.convertStringToSeed(phrase);
			
			//Not set yet..
			MinimaDB.getDB().getUserDB().setBasePrivatePhrase(phrase);
			MinimaDB.getDB().getUserDB().setBasePrivateSeed(seed.to0xString());
		}
		
		//Get the base private seed..
		String basepriv = MinimaDB.getDB().getUserDB().getBasePrivateSeed();
		MinimaDB.getDB().getWallet().initBaseSeed(new MiniData(basepriv));
		
		//Calculate the User hashrate.. start her up as seems to make a difference.. initialises..
		TxPoWMiner.calculateHashRate(new MiniNumber(10000));
		
		//Now do the actual check..
		MiniNumber hashcheck = new MiniNumber("250000");
		MiniNumber hashrate_old = TxPoWMiner.calculateHashRate(hashcheck);
		MiniNumber hashrate = TxPoWMiner.calculateHashSpeed(hashcheck);
		MinimaDB.getDB().getUserDB().setHashRate(hashrate);
		MinimaLogger.log("Calculate device hash rate : "+hashrate.div(MiniNumber.MILLION).setSignificantDigits(4)+" MHs");
		
		//Create the Initial Key Set
		try {
			mInitKeysCreated = MinimaDB.getDB().getWallet().initDefaultKeys(2);
		}catch(Exception exc) {
			MinimaLogger.log(exc.toString());
		}
		
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
		
		//Start MDS
		mMDS = new MDSManager();
		
		//New Send POll Manager
		mSendPoll = new SendPollManager();
		
		//Simulate traffic message
		AUTOMINE_TIMER = MiniNumber.THOUSAND.div(GlobalParams.MINIMA_BLOCK_SPEED).getAsLong();
		mTxPoWMiner.PostTimerMessage(new TimerMessage(AUTOMINE_TIMER, TxPoWMiner.TXPOWMINER_MINEPULSE));
		
		//Set the PULSE message timer.
		PostTimerMessage(new TimerMessage(GeneralParams.USER_PULSE_FREQ, MAIN_PULSE));
		
		//Clean the DB (delete old records)
		if(GeneralParams.GENESIS) {
			//Do sooner as stores the genesis Txn..
			PostTimerMessage(new TimerMessage(10 * 1000, MAIN_CLEANDB_RAM));
		}else {
			PostTimerMessage(new TimerMessage(60 * 1000, MAIN_CLEANDB_RAM));
		}
		PostTimerMessage(new TimerMessage(60 * 1000, MAIN_CLEANDB_SQL));
		
		//Store the IC User - do fast first time - 30 seconds in.. then every 8 hours
		PostTimerMessage(new TimerMessage(1000*30, MAIN_INCENTIVE));
		
		//Debug Checker
		PostTimerMessage(new TimerMessage(CHECKER_TIMER, MAIN_CHECKER));
		
		//Reset Network stats every 24 hours
		PostTimerMessage(new TimerMessage(NETRESET_TIMER, MAIN_NETRESET));
		
		//AutoBackup - do one in 10 minutes then every 24 hours
		PostTimerMessage(new TimerMessage(1000 * 60 * 10, MAIN_AUTOBACKUP));
		
		//Quick Clean up..
		System.gc();
	}
	
	/**
	 * Used after a Restore
	 */
	public void setHasShutDown() {
		mShuttingdown = true;
	}
	
	public boolean isShuttingDown() {
		return mShuttingdown;
	}
	
	public boolean isRestoring() {
		return mRestoring;
	}
	
	public void shutdown() {
		//Are we already shutting down..
		if(mShuttingdown) {
			return;
		}
		
		MinimaLogger.log("Shut down started..");
		
		//we are shutting down
		mShuttingdown = true;
		
		try {
			
			//Tell the wallet - in case we are creating default keys
			MinimaDB.getDB().getWallet().shuttingDown();
			
			//Shut down the network
			shutdownGenProcs();
			
			//Stop the main TxPoW processor
			MinimaLogger.log("Waiting for TxPoWProcessor shutdown");
			mTxPoWProcessor.stopMessageProcessor();
			mTxPoWProcessor.waitToShutDown(false);
			
			//Now backup the  databases
			MinimaLogger.log("Saving all db");
			MinimaDB.getDB().saveAllDB();
					
			//Stop this..
			stopMessageProcessor();
			
			//Wait for it..
			MinimaLogger.log("Waiting for Main thread shutdown");
			waitToShutDown(true);
		
			MinimaLogger.log("Shut down completed OK..");
			
		}catch(Exception exc) {
			MinimaLogger.log("ERROR Shutting down..");
			MinimaLogger.log(exc);
		}
	}
	
	public void restoreReady() {
		//we are about to restore..
		mRestoring = true;
		
		//Shut down the network
		shutdownGenProcs();
		
		//Stop the main TxPoW processor
		mTxPoWProcessor.stopMessageProcessor();
		mTxPoWProcessor.waitToShutDown(false);	
	}
	
	public void archiveResetReady(boolean zResetWallet) {
		//we are about to restore..
		mRestoring = true;
				
		//Shut most of the processors down
		shutdownGenProcs();
		
		//Delete old files.. and reset to new
		MinimaDB.getDB().getTxPoWDB().getSQLDB().saveDB();
		MinimaDB.getDB().getTxPoWDB().getSQLDB().getSQLFile().delete();
		
		MinimaDB.getDB().getArchive().saveDB();
		MinimaDB.getDB().getArchive().getSQLFile().delete();
		
		//Are we deleting the wallet..
		if(zResetWallet) {
			MinimaDB.getDB().getWallet().saveDB();
			MinimaDB.getDB().getWallet().getSQLFile().delete();
		}
		
		//Reload the SQL dbs
		MinimaDB.getDB().loadArchiveAndTxPoWDB(zResetWallet);
		
		//Reset these 
		MinimaDB.getDB().resetCascadeAndTxPoWTree();
	}
	
	private void shutdownGenProcs() {
		
		//No More timer Messages
		TimerProcessor.stopTimerProcessor();
				
		//Shut down the network
		mNetwork.shutdownNetwork();
		
		//Shut down Maxima
		mMaxima.shutdown();
		
		//ShutDown MDS
		mMDS.shutdown();
				
		//Stop the Miner
		mTxPoWMiner.stopMessageProcessor();
		
		//Stop sendPoll
		mSendPoll.stopMessageProcessor();
		
		//Wait for the networking to finish
		long timewaited=0;
		while(!mNetwork.isShutDownComplete()) {
			try {Thread.sleep(250);} catch (InterruptedException e) {}
			timewaited+=250;
			if(timewaited>10000) {
				MinimaLogger.log("Network shutdown took too long..");
				break;
			}
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
	
	//Every 50 seconds - the normal blockspeed
	public void setNormalAutoMineSpeed() {
		AUTOMINE_TIMER = 1000 * 50;
	}
	
	//Every 500 seconds - for Android when not plugged in
	public void setLowPowAutoMineSpeed() {
		AUTOMINE_TIMER = 1000 * 500;
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
	
	public MDSManager getMDSManager() {
		return mMDS;
	}
	
	public SendPollManager getSendPoll() {
		return mSendPoll;
	}
		
	private void doGenesis() {
		
		//Create a new address - to receive the genesis funds..
		ScriptRow scrow = MinimaDB.getDB().getWallet().createNewSimpleAddress(true);
		
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
			
		}else if(zMessage.getMessageType().equals(MAIN_CLEANDB_RAM)) {
			
			//Clean up the RAM Memory
			System.gc();
			
			//Do some house keeping on the DB
			MinimaDB.getDB().getTxPoWDB().cleanDBRAM();
			
			//Now save the state - in case system crashed..
			MinimaDB.getDB().saveState();
			
			//Do it again..
			PostTimerMessage(new TimerMessage(CLEANDB_RAM_TIMER, MAIN_CLEANDB_RAM));
		
		}else if(zMessage.getMessageType().equals(MAIN_CLEANDB_SQL)) {
			
			//Do some house keeping on the DB
			MinimaDB.getDB().getTxPoWDB().cleanDBSQL();
			
			//Same with the ArchiveDB
			MinimaDB.getDB().getArchive().cleanDB();
			
			//Do it again..
			PostTimerMessage(new TimerMessage(CLEANDB_SQL_TIMER, MAIN_CLEANDB_SQL));
			
		}else if(zMessage.getMessageType().equals(MAIN_PULSE)) {
			
			//Create Pulse Message
			Pulse pulse = Pulse.createPulse();
		
			//And send it to all your peers..
			NIOManager.sendNetworkMessageAll(NIOMessage.MSG_PULSE, pulse);
			
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

		}else if(zMessage.getMessageType().equals(MAIN_AUTOBACKUP)) {
			
			//Are we backing up..
			if(MinimaDB.getDB().getUserDB().isAutoBackup()) {
			
				//Create a backup command..
				JSONArray res = Command.runMultiCommand("backup");
				
				//Output
				MinimaLogger.log("AUTOBACKUP : "+res.toString());
			
			}
			
			//And Again..
			PostTimerMessage(new TimerMessage(AUTOBACKUP_TIMER, MAIN_AUTOBACKUP));
			
		}else if(zMessage.getMessageType().equals(MAIN_NETRESET)) {
			
			//Reset the networking stats
			Main.getInstance().getNIOManager().getTrafficListener().reset();
			
			//Reset Network stats every 24 hours
			PostTimerMessage(new TimerMessage(NETRESET_TIMER, MAIN_NETRESET));
			
		}else if(zMessage.getMessageType().equals(MAIN_SHUTDOWN)) {
			
			shutdown();
			
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
	 * Post a network message to the webhook / MDS / Android listeners
	 */
	public void PostNotifyEvent(String zEvent, JSONObject zData) {
		
		//Create the JSON Message
		JSONObject notify = new JSONObject();
		notify.put("event", zEvent);
		notify.put("data", zData);
		
		if(getNetworkManager() != null) {
			//And post
			getNetworkManager().getNotifyManager().PostEvent(notify);
		}
		
		//Tell the MDS..
		if(getMDSManager() != null) {
			Message poll = new Message(MDSManager.MDS_POLLMESSAGE);
			poll.addObject("poll", notify);
			poll.addObject("to", "*");
			
			getMDSManager().PostMessage(poll);
		}
	}
	
}
