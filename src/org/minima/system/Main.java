
package org.minima.system;

import java.util.ArrayList;

import org.minima.GlobalParams;
import org.minima.database.prefs.UserPrefs;
import org.minima.system.brains.BackupManager;
import org.minima.system.brains.ConsensusBackup;
import org.minima.system.brains.ConsensusHandler;
import org.minima.system.brains.SendManager;
import org.minima.system.input.InputHandler;
import org.minima.system.network.NetworkHandler;
import org.minima.system.txpow.TxPoWMiner;
import org.minima.utils.MinimaLogger;
import org.minima.utils.SQLHandler;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;

public class Main extends MessageProcessor {

	/**
	 * Retrieve the Main handler.. from where you can retrieve everything else..
	 */
	private static Main mMainHandler;
	public static Main getMainHandler() {
		return mMainHandler;
	}
	
	public static final String SYSTEM_STARTUP 		= "SYSTEM_STARTUP";
	
	public static final String SYSTEM_INIT 		    = "SYSTEM_INIT";
	
	public static final String SYSTEM_SHUTDOWN 		= "SYSTEM_SHUTDOWN";
	public static final String SYSTEM_FULLSHUTDOWN 	= "SYSTEM_FULLSHUTDOWN";
	
	public static final String SYSTEM_EVENT 		= "SYSTEM_EVENT";
		
	/**
	 * The Input handler
	 */
	private InputHandler mInput;
	
	/**
	 * The Network manager
	 */
	private NetworkHandler mNetwork;
	
	/**
	 * The Transaction Miner
	 */
	private TxPoWMiner mTXMiner;
	
	/**
	 * The Main bottleneck thread that calculates the actual situation
	 */
	private ConsensusHandler mConsensus;
	
	/**
	 * The Enterprise Send Poller
	 */
	private SendManager mSendManager;
	
	/**
	 * The Backup Manager - runs in a separate thread
	 */
	private BackupManager mBackup;

	/**
	 * User Preferences
	 */
	UserPrefs mUserPrefs;
	
	/**
	 * Default nodes to connect to
	 */
	public boolean mAutoConnect        = false;
	ArrayList<String> mAutoConnectList = new ArrayList<>();
	
	/**
	 * When did this node start up..
	 */
	long mNodeStartTime;
	
	/**
	 * Main Constructor
	 * @param zPort
	 * @param zGenesis
	 */
	public Main(String zHost, int zPort, String zConfFolder) {
		super("MAIN");
		
		mMainHandler = this;
		
		//What time do we start..
		mNodeStartTime = System.currentTimeMillis();
		
		//Backup manager
		mBackup     = new BackupManager(zConfFolder);

		//Set the TeMP folder
		System.setProperty("java.io.tmpdir",BackupManager.getTempFolder().getAbsolutePath());
		
		//The guts..
		mInput 		= new InputHandler();
		mNetwork 	= new NetworkHandler(zHost, zPort);
		mTXMiner 	= new TxPoWMiner();
		mConsensus  = new ConsensusHandler();
		mSendManager = new SendManager();
		
		/**
		 * Introduction..
		 */
		MinimaLogger.setMainHandler(mConsensus);
		
		MinimaLogger.log("**********************************************");
		MinimaLogger.log("*  __  __  ____  _  _  ____  __  __    __    *");
		MinimaLogger.log("* (  \\/  )(_  _)( \\( )(_  _)(  \\/  )  /__\\   *");
		MinimaLogger.log("*  )    (  _)(_  )  (  _)(_  )    (  /(__)\\  *");
		MinimaLogger.log("* (_/\\/\\_)(____)(_)\\_)(____)(_/\\/\\_)(__)(__) *");
		MinimaLogger.log("*                                            *");
		MinimaLogger.log("**********************************************");
		MinimaLogger.log("Welcome to Minima. For assistance type help. Then press enter.");
		MinimaLogger.log("Minima files : "+zConfFolder);
		MinimaLogger.log("Minima version "+GlobalParams.MINIMA_VERSION);
		
		//Load the UserPrefs
		mUserPrefs = new UserPrefs();
		mUserPrefs.loadDB(mBackup.getUserPrefs());
	}
	
	public void setAutoConnect(boolean zAuto) {
		mAutoConnect = zAuto;
	}
	
	public void clearAutoConnectHostPort(String zHostPort) {
		mAutoConnectList.clear();
	}
	
	public void addAutoConnectHostPort(String zHostPort) {
		mAutoConnectList.add(zHostPort);
	}
	
	public long getNodeStartTime() {
		return mNodeStartTime;
	}
	
	public void setTrace(boolean zTraceON) {
		setLOG(zTraceON);
		
		mConsensus.setLOG(zTraceON);
		mNetwork.PostMessage(new Message(NetworkHandler.NETWORK_TRACE).addBoolean("trace", zTraceON));
		mTXMiner.setLOG(zTraceON);
		mInput.setLOG(zTraceON);
		mBackup.setLOG(zTraceON);
	}
	
	public InputHandler getInputHandler() {
		return mInput;
	}
	
	public NetworkHandler getNetworkHandler() {
		return mNetwork;
	}
	
	public ConsensusHandler getConsensusHandler() {
		return mConsensus;
	}
	
	public SendManager getSendManaManager() {
		return mSendManager;
	}
	
	public BackupManager getBackupManager() {
		return mBackup;
	}
	
	public TxPoWMiner getMiner() {
		return mTXMiner;
	}
		
	public UserPrefs getUserPrefs() {
		return mUserPrefs;
	}
	
	public void privateChain(boolean zNeedGenesis) {
		//Set the Database backup manager
		getConsensusHandler().setBackUpManager();
		
		if(zNeedGenesis){
			//Sort the genesis Block
			mConsensus.genesis();
		}
		
		//Tell miner we are auto mining..
		mTXMiner.setAutoMining(true);
	}
	
	public void setAutoMine() {
		//Tell miner we are auto mining..
		mTXMiner.setAutoMining(true);
	}
	
	public void setRequireNoInitialSync() {
		mConsensus.setInitialSyncComplete();
	}
		
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		if (zMessage.isMessageType(SYSTEM_STARTUP) ) {
			//Set the Database backup manager
			getConsensusHandler().setBackUpManager();
			
			//Restore..
			getConsensusHandler().PostMessage(ConsensusBackup.CONSENSUSBACKUP_RESTORE);
			
		}else if ( zMessage.isMessageType(SYSTEM_INIT) ) {
			
			//Start the network..	
			mNetwork.PostMessage(NetworkHandler.NETWORK_STARTUP);

			//And do we do an automatic logon..
			if(mAutoConnect) {
				//Connect to the the list of auto connect
				for(String hostport : mAutoConnectList) {
					int div     = hostport.indexOf(":");
					String host = hostport.substring(0,div);
					int port    = Integer.parseInt(hostport.substring(div+1));
					
					//Send a TimedMessage..
					Message connect  = new Message(NetworkHandler.NETWORK_CONNECT)
							.addInteger("port", port).addString("host", host);
					getNetworkHandler().PostMessage(connect);
				
					//Small Pause.. 10 seconds..
					Thread.sleep(10000);
				}
			}
			
		}else if ( zMessage.isMessageType(SYSTEM_SHUTDOWN) ) {
			
			//make a backup and shutdown message
			Message backshut = new Message(ConsensusBackup.CONSENSUSBACKUP_BACKUP);
			backshut.addBoolean("shutdown", true);
			
			//Keep the response message
			InputHandler.addResponseMesage(backshut, zMessage);
			
			//Save all the user details..
			getConsensusHandler().PostMessage(backshut);
			
		}else if ( zMessage.isMessageType(SYSTEM_FULLSHUTDOWN) ) {
			
			//Savew ther UserPrefs
			mUserPrefs.saveDB(mBackup.getUserPrefs());
			
			//Notify Listeners..
			mConsensus.updateListeners(new Message(ConsensusHandler.CONSENSUS_NOTIFY_QUIT));
			
			//Gracefull shutdown..
			mNetwork.PostMessage(NetworkHandler.NETWORK_SHUTDOWN);
			
			//Shut the Send Manager
			mSendManager.PostMessage(SendManager.SENDMANAGER_SHUTDOWN);
			
			//Shut down the individual systems..
			mInput.stopMessageProcessor();
			mTXMiner.stopMessageProcessor();
			mConsensus.stopMessageProcessor();
			
			//Wait for the backup machine to finish..
			while(mBackup.getSize()>0) {
				MinimaLogger.log("Backup Manager NOT Finished.. waiting.. ");
				Thread.sleep(1000);
			}
			mBackup.stopMessageProcessor();
			
			//Shut the database.
			SQLHandler.CloseSQL();
			
			//Wait a second..
			Thread.sleep(1000);
			
			//And shut this down too..
			stopMessageProcessor();
			
			//It's over..
			InputHandler.endResponse(zMessage, true, "Minima Stopped. Bye Bye..");
					
		}else {
			//Unknown Message..
			MinimaLogger.log("Unknown Message sent to main handler "+zMessage);
		}
	}

}
