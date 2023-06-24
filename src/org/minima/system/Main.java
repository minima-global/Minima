package org.minima.system;

import java.io.File;
import java.util.ArrayList;

import org.minima.Minima;
import org.minima.database.MinimaDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.userprefs.UserDB;
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
import org.minima.system.network.p2p.P2PFunctions;
import org.minima.system.network.webhooks.NotifyManager;
import org.minima.system.params.GeneralParams;
import org.minima.system.params.GlobalParams;
import org.minima.system.sendpoll.SendPollManager;
import org.minima.utils.MiniFile;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageListener;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;
import org.minima.utils.messages.TimerProcessor;
import org.minima.utils.ssl.SSLManager;

public class Main extends MessageProcessor {

	public static boolean STARTUP_DEBUG_LOGS = false;
	
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
	 * Have we told listener to shutdown..
	 */
	private boolean mShutDownSentToListener = false;
	
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
	
	public static final String MAIN_SYSTEMCLEAN 	= "MAIN_SYSTEMCLEAN";
	long SYSTEMCLEAN_TIMER	= 1000 * 60 * 5;
	
	public static final String MAIN_AUTOBACKUP_MYSQL 	= "MAIN_AUTOBACKUP_MYSQL";
	long MAIN_AUTOBACKUP_MYSQL_TIMER					= 1000 * 60 * 60 * 2;
	
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
	 * Create all the initial Keys
	 */
	public static final String MAIN_INIT_KEYS 	= "MAIN_INIT_KEYS";
	long INIT_KEYS_TIMER = 1000 * 15;
	
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
	 * The Web Hooks for Minima messages
	 */
	NotifyManager mNotifyManager;
	
	/**
	 * Are we shutting down..
	 */
	boolean mShuttingdown = false;
	
	/**
	 * Are we restoring..
	 */
	boolean mRestoring = false;
	
	/**
	 * Are we syncing an IBD
	 */
	boolean mSyncIBD = false;
	
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
	
		if(STARTUP_DEBUG_LOGS) {
			MinimaLogger.log("MAIN init.. start");
		}
		
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
		if(STARTUP_DEBUG_LOGS) {
			MinimaLogger.log("MinimaDB create.. start");
		}
		MinimaDB.createDB();
		if(STARTUP_DEBUG_LOGS) {
			MinimaLogger.log("MinimaDB create.. finish");
		}
		
		//Load the Databases
		if(STARTUP_DEBUG_LOGS) {
			MinimaLogger.log("Load all DB.. start");
		}
		MinimaDB.getDB().loadAllDB();
		if(STARTUP_DEBUG_LOGS) {
			MinimaLogger.log("Load all DB.. finish");
		}
		
		//Are we in Slave node mode
		boolean slavemode = MinimaDB.getDB().getUserDB().isSlaveNode();
		if(slavemode) {
			GeneralParams.CONNECT_LIST 			= MinimaDB.getDB().getUserDB().getSlaveNodeHost();
        	GeneralParams.P2P_ENABLED 			= false;
            GeneralParams.TXBLOCK_NODE 			= true;
            GeneralParams.NO_SYNC_IBD 			= true;
            GeneralParams.IS_ACCEPTING_IN_LINKS = false;
            MinimaLogger.log("Slave Mode ENABLED master:"+GeneralParams.CONNECT_LIST);
		}
		
		//Create the SSL Keystore..
		if(STARTUP_DEBUG_LOGS) {
			MinimaLogger.log("SSL Key.. start");
		}
		SSLManager.makeKeyFile();
		if(STARTUP_DEBUG_LOGS) {
			MinimaLogger.log("SSL Key.. finish");
		}
		//Calculate the User hashrate.. start her up as seems to make a difference.. initialises..
		TxPoWMiner.calculateHashRateOld(new MiniNumber(10000));
		
		//Now do the actual check..
		MiniNumber hashcheck 	= new MiniNumber("250000");
		MiniNumber hashrate 	= TxPoWMiner.calculateHashSpeed(hashcheck);
		MinimaDB.getDB().getUserDB().setHashRate(hashrate);
		MinimaLogger.log("Calculate device hash rate : "+hashrate.div(MiniNumber.MILLION).setSignificantDigits(4)+" MHs");
		
		//Create the Initial Key Set
		try {
			mInitKeysCreated = MinimaDB.getDB().getWallet().initDefaultKeys(2);
		}catch(Exception exc) {
			MinimaLogger.log(exc.toString());
		}
		
		//Notification of Events
		mNotifyManager = new NotifyManager();
				
		//Start the engine..
		mTxPoWProcessor = new TxPoWProcessor();
		
		//Recalc Tree if too large
		try {
			if(MinimaDB.getDB().getTxPoWTree().getHeaviestBranchLength() > 1200) {
				MinimaLogger.log("Large tree.. recalculating..");
				mTxPoWProcessor.onStartUpRecalc();
				
				//For now..
				MinimaDB.getDB().saveTxPoWTree();
			}	
		}catch(Exception exc) {
			MinimaLogger.log(exc);
		}
		
		//Create the TxpowMiner
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
			PostTimerMessage(new TimerMessage(3 * 60 * 1000, MAIN_CLEANDB_RAM));
		}
		PostTimerMessage(new TimerMessage(10 * 60 * 1000, MAIN_CLEANDB_SQL));
		
		//System Clean..
		PostTimerMessage(new TimerMessage(SYSTEMCLEAN_TIMER, MAIN_SYSTEMCLEAN));
		
		//Debug Checker
		PostTimerMessage(new TimerMessage(CHECKER_TIMER, MAIN_CHECKER));
		
		//Init Keys
		PostTimerMessage(new TimerMessage(1000 * 30, MAIN_INIT_KEYS));
				
		//Reset Network stats every 24 hours
		PostTimerMessage(new TimerMessage(NETRESET_TIMER, MAIN_NETRESET));
		
		//AutoBackup - do one in 5 minutes then every 24 hours
		PostTimerMessage(new TimerMessage(1000 * 60 * 5, MAIN_AUTOBACKUP));
		
		//MYSQL AutoBackup - do one in 5 minutes then every 2 hours
		PostTimerMessage(new TimerMessage(1000 * 60 * 5, MAIN_AUTOBACKUP_MYSQL));
				
		//Quick Clean up..
		System.gc();
		
		//Check slavenode status
		if(GeneralParams.TXBLOCK_NODE) {
			
			if(GeneralParams.CONNECT_LIST.indexOf(",")!=-1) {
				//Can only connect to 1 host
				MinimaLogger.log("[!] Can ONLY connect to 1 host in slave mode.. stopping");
				Runtime.getRuntime().exit(1);
			}
			
			MinimaLogger.log("Running in slave mode. Will Connect to "+GeneralParams.CONNECT_LIST);
		}
	}
	
	/**
	 * Are we syncing an IBD
	 */
	public void setSyncIBD(boolean zSync) {
		if(GeneralParams.IBDSYNC_LOGS) {
			MinimaLogger.log("SYNC IBD LOCK : "+zSync);
		}
		mSyncIBD = zSync;
	}
	
	public boolean isSyncIBD() {
		return mSyncIBD;
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
	
	public boolean isShuttongDownOrRestoring() {
		return mShuttingdown || mRestoring;
	}
	
	public void shutdown() {
		shutdown(false);
	}
	
	public void shutdown(boolean zCompact) {
		//Are we already shutting down..
		if(mShuttingdown) {
			return;
		}
		
		if(zCompact) {
			MinimaLogger.log("Shut down started.. Compacting All Databases");
		}else {
			MinimaLogger.log("Shut down started..");
		}
		
		//we are shutting down
		mShuttingdown = true;
		
		try {
			
			//Tell the wallet - in case we are creating default keys
			MinimaDB.getDB().getWallet().shuttingDown();
			
			//Shut down the network
			shutdownGenProcs();
			
			//Stop the main TxPoW processor
			shutdownFinalProcs();
			
			//Now backup the  databases
			MinimaLogger.log("Saving all db");
			MinimaDB.getDB().saveAllDB(zCompact);
					
			//Stop this..
			stopMessageProcessor();
			
			//Wait for it..
			MinimaLogger.log("Main thread shutdown");
			waitToShutDown();
			
			MinimaLogger.log("Shut down completed OK..");
			
			//Tell listener..
			NotifyMainListenerOfShutDown();
			
		}catch(Exception exc) {
			MinimaLogger.log("ERROR Shutting down..");
			MinimaLogger.log(exc);
		}
	}
	
	public void NotifyMainListenerOfShutDown() {
		
		//Have we done this already
		if(mShutDownSentToListener) {
			return;
		}
		mShutDownSentToListener = true;
		
		//Send them a message
		try {
			NotifyMainListenerOnly("SHUTDOWN");
		} catch (Exception e) {
			MinimaLogger.log(e);
		}
	}
	
	public void restoreReady() {
		//we are about to restore..
		mRestoring = true;
		
		//Shut down the network
		shutdownGenProcs();
		
		//Stop the main TxPoW processor
		shutdownFinalProcs();
	}
	
	public void restoreReadyForSync() {
		
		//Restart the Processor
		mTxPoWProcessor = new TxPoWProcessor();
		
		//Reload the DBs..
		MinimaDB.getDB().loadDBsForRestoreSync();
	}
	
	public void archiveResetReady(boolean zResetWallet) {
		archiveResetReady(zResetWallet, true);
	}
	
	public void archiveResetReady(boolean zResetWallet, boolean zResetCascadeTree) {
		//we are about to restore..
		mRestoring = true;
				
		//Shut most of the processors down
		shutdownGenProcs();
		
		//Delete old files.. and reset to new
		MinimaDB.getDB().getTxPoWDB().getSQLDB().saveDB(false);
		if(zResetCascadeTree) {
			MinimaDB.getDB().getTxPoWDB().getSQLDB().getSQLFile().delete();
		}
		
		MinimaDB.getDB().getArchive().saveDB(false);
		MinimaDB.getDB().getArchive().getSQLFile().delete();
		
		//Are we deleting the wallet..
		if(zResetWallet) {
			MinimaDB.getDB().getWallet().saveDB(false);
			MinimaDB.getDB().getWallet().getSQLFile().delete();
		}
		
		//Reload the SQL dbs
		MinimaDB.getDB().loadArchiveAndTxPoWDB(zResetWallet);
		
		if(zResetCascadeTree) {
			//Reset these 
			MinimaDB.getDB().resetCascadeAndTxPoWTree();
		}
	}
	
	private void shutdownGenProcs() {
		
		//No More timer Messages
		TimerProcessor.stopTimerProcessor();
				
		//Shut down the network
		mNetwork.shutdownNetwork();
		
		//Shut down Maxima
		mMaxima.shutdown();
				
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
	
	public void shutdownFinalProcs() {
				
		//ShutDown MDS
		MinimaLogger.log("Shutdown MDS..");
		mMDS.shutdown();
		
		//Shut down the Notify Manager
		mNotifyManager.shutDown();
				
		//Stop the main TxPoW processor
		MinimaLogger.log("TxPoWProcessor shutdown..");
		mTxPoWProcessor.stopMessageProcessor();
		mTxPoWProcessor.waitToShutDown();
	}
	
	/**
	 * USed when Syncing to clear memory
	 */
	public void resetMemFull() {
		MinimaLogger.log("System full memory clean..");
		
		//Reset all the DBs..
		MinimaDB.getDB().fullDBRestartMemFree();
		
		//Stop the main TxPoW processor
		mTxPoWProcessor.stopMessageProcessor();
		mTxPoWProcessor.waitToShutDown();
		
		//Now reset the main processor..
		mTxPoWProcessor = new TxPoWProcessor();
		
		//And system clean 
		System.gc();
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
	
	public NotifyManager getNotifyManager() {
		return mNotifyManager;
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
	
	public boolean getAllKeysCreated() {
		return mInitKeysCreated;
	}
	
	public int getAllDefaultKeysSize() {
		return MinimaDB.getDB().getWallet().getDefaultKeysNumber();
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
			
		}else if(zMessage.getMessageType().equals(MAIN_SYSTEMCLEAN)) {
			
			//Do it again..
			PostTimerMessage(new TimerMessage(SYSTEMCLEAN_TIMER, MAIN_SYSTEMCLEAN));
			
			//Clean up the RAM Memory
			System.gc();
			
		}else if(zMessage.getMessageType().equals(MAIN_CLEANDB_RAM)) {
			
			//Do it again..
			PostTimerMessage(new TimerMessage(CLEANDB_RAM_TIMER, MAIN_CLEANDB_RAM));
			
			//Do some house keeping on the DB
			MinimaDB.getDB().getTxPoWDB().cleanDBRAM();
			
			//Now save the state - in case system crashed..
			MinimaDB.getDB().saveState();
			
		}else if(zMessage.getMessageType().equals(MAIN_AUTOBACKUP_MYSQL)) {
			
			//Do it again..
			PostTimerMessage(new TimerMessage(MAIN_AUTOBACKUP_MYSQL_TIMER, MAIN_AUTOBACKUP_MYSQL));
			
			UserDB udb = MinimaDB.getDB().getUserDB();
			
			//Are we enabled..
			if(udb.getAutoBackupMySQL()) {
				
				String backupcommand = "mysql host:"+udb.getAutoMySQLHost()
								+" database:"+udb.getAutoMySQLDB()
								+" user:"+udb.getAutoMySQLUser()
								+" password:"+udb.getAutoMySQLPassword()
								+" action:update";
				
				//Run a mysql Backup of the archive data..
				JSONArray res 	= Command.runMultiCommand(backupcommand);
				JSONObject json = (JSONObject) res.get(0); 
				boolean status  = (boolean) json.get("status");
				
				//Output
				if(!status) {
					MinimaLogger.log("[ERROR] MYSQL AUTOBACKUP "+json.getString("error"));
				}else {
					JSONObject response = (JSONObject) json.get("response");
					MinimaLogger.log("MYSQL AUTOBACKUP OK "+response.toString());
				}
			}
			
		}else if(zMessage.getMessageType().equals(MAIN_CLEANDB_SQL)) {
			
			//Do it again..
			PostTimerMessage(new TimerMessage(CLEANDB_SQL_TIMER, MAIN_CLEANDB_SQL));
			
			//Do some house keeping on the DB
			MinimaDB.getDB().getTxPoWDB().cleanDBSQL();
			
			//Same with the ArchiveDB - if not running an archive node
			MinimaDB.getDB().getArchive().checkForCleanDB();
			
		}else if(zMessage.getMessageType().equals(MAIN_PULSE)) {
			
			//And then wait again..
			PostTimerMessage(new TimerMessage(GeneralParams.USER_PULSE_FREQ, MAIN_PULSE));
			
			//Are we a Slavenode - have no transactions.. use TXBLOCKMINE msg instead
			if(GeneralParams.TXBLOCK_NODE) {
				return;
			}
			
			//Create Pulse Message
			Pulse pulse = Pulse.createPulse();
		
			//And send it to all your peers..
			NIOManager.sendNetworkMessageAll(NIOMessage.MSG_PULSE, pulse);
			
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
			
			//And Again..
			PostTimerMessage(new TimerMessage(AUTOBACKUP_TIMER, MAIN_AUTOBACKUP));
			
			//Are we backing up..
			if(MinimaDB.getDB().getUserDB().isAutoBackup()) {
			
				//Create a backup command..
				JSONArray res = Command.runMultiCommand("backup");
				
				//Output
				MinimaLogger.log("AUTOBACKUP : "+res.toString());
			}
			
			//Clear the Invalid Peers
			P2PFunctions.clearInvalidPeers();
			
			//Recalculate the hash speed..
			MiniNumber hashcheck 	= new MiniNumber("250000");
			MiniNumber hashrate 	= TxPoWMiner.calculateHashSpeed(hashcheck);
			MinimaDB.getDB().getUserDB().setHashRate(hashrate);
			MinimaLogger.log("Re-Calculate device hash rate : "+hashrate.div(MiniNumber.MILLION).setSignificantDigits(4)+" MHs");
			
		}else if(zMessage.getMessageType().equals(MAIN_NETRESET)) {
			
			//Reset the networking stats
			Main.getInstance().getNIOManager().getTrafficListener().reset();
			
			//Reset Network stats every 24 hours
			PostTimerMessage(new TimerMessage(NETRESET_TIMER, MAIN_NETRESET));
			
		}else if(zMessage.getMessageType().equals(MAIN_SHUTDOWN)) {
			
			shutdown();
		
		}else if(zMessage.getMessageType().equals(MAIN_INIT_KEYS)) {
			
			//Check the Default keys
			if(!mInitKeysCreated) {
				try {
					mInitKeysCreated = MinimaDB.getDB().getWallet().initDefaultKeys(8);
					if(mInitKeysCreated) {
						MinimaLogger.log("All default getaddress keys created..");
					}
				}catch(Exception exc) {
					MinimaLogger.log(exc);
				}
			}
			
			//Check again..
			if(!mInitKeysCreated) {
				PostTimerMessage(new TimerMessage(INIT_KEYS_TIMER, MAIN_INIT_KEYS));
			}
			
		}else if(zMessage.getMessageType().equals(MAIN_CHECKER)) {
			
			//Check again..
			PostTimerMessage(new TimerMessage(CHECKER_TIMER, MAIN_CHECKER));
			
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
		
		if(getNotifyManager() != null) {
			
			//And post
			getNotifyManager().PostEvent(notify);
		}
		
		//Tell the MDS..
		if(getMDSManager() != null) {
			Message poll = new Message(MDSManager.MDS_POLLMESSAGE);
			poll.addObject("poll", notify);
			poll.addObject("to", "*");
			
			getMDSManager().PostMessage(poll);
		}
	}
	
	/**
	 * Send a message ONLY to the LIstener..
	 */
	public static void NotifyMainListenerOnly(String zMessage) throws Exception {
		//Notify
		if(getMinimaListener() != null) {
			
			//Create the JSON Message
			JSONObject notify = new JSONObject();
			notify.put("event", zMessage);
			notify.put("data", new JSONObject());
			
			Message msg = new Message(NotifyManager.NOTIFY_POST);
			msg.addObject("notify", notify);
			
			//Notify them that something is happening..
			getMinimaListener().processMessage(msg);
		}
	}
}
