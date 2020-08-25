
package org.minima.system;

import java.util.ArrayList;

import org.minima.GlobalParams;
import org.minima.system.brains.BackupManager;
import org.minima.system.brains.ConsensusBackup;
import org.minima.system.brains.ConsensusHandler;
import org.minima.system.input.InputHandler;
import org.minima.system.network.NetworkHandler;
import org.minima.system.txpow.TxPoWMiner;
import org.minima.utils.MinimaLogger;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;

public class Main extends MessageProcessor {

	/**
	 * Retrieve the input handler.. used by the MiniLibs..
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
	 * The Backup Manager - runs in a separate thread
	 */
	private BackupManager mBackup;
	
	/**
	 * Are we creating a network from scratch
	 */
	boolean mGenesis = false;

	/**
	 * Default nodes to connect to
	 */
	public boolean mAutoConnect        = false;
	ArrayList<String> mAutoConnectList = new ArrayList<>();
	
//	public String mAutoHost 	= "";
//	public int mAutoPort    	= 0;
	
	/**
	 * When did this node start up..
	 */
	long mNodeStartTime;
	
	/**
	 * Main COnstructor
	 * @param zPort
	 * @param zGenesis
	 */
	public Main(String zHost, int zPort, boolean zGenesis, String zConfFolder) {
		super("MAIN");
		
		mMainHandler = this;
		
		//What time do we start..
		mNodeStartTime = System.currentTimeMillis();
		
		/**
		 * Introduction..
		 */
		MinimaLogger.log("**********************************************");
		MinimaLogger.log("*  __  __  ____  _  _  ____  __  __    __    *");
		MinimaLogger.log("* (  \\/  )(_  _)( \\( )(_  _)(  \\/  )  /__\\   *");
		MinimaLogger.log("*  )    (  _)(_  )  (  _)(_  )    (  /(__)\\  *");
		MinimaLogger.log("* (_/\\/\\_)(____)(_)\\_)(____)(_/\\/\\_)(__)(__) *");
		MinimaLogger.log("*                                            *");
		MinimaLogger.log("**********************************************");
		
		//Backup manager
		mBackup     = new BackupManager(this,zConfFolder);

		//Set the TeMP folder
		System.setProperty("java.io.tmpdir",BackupManager.getTempFolder().getAbsolutePath());
		
		//The guts..
		mInput 		= new InputHandler(this);
		mNetwork 	= new NetworkHandler(this, zHost, zPort);
		mTXMiner 	= new TxPoWMiner(this);
		mConsensus  = new ConsensusHandler(this);
		
		//Are we the genesis
		mGenesis 	= zGenesis;
		
		//Some info..
		MinimaLogger.log("Minima files : "+zConfFolder);
		MinimaLogger.log("Minima version "+GlobalParams.MINIMA_VERSION);
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
	
	public BackupManager getBackupManager() {
		return mBackup;
	}
	
	public TxPoWMiner getMiner() {
		return mTXMiner;
	}
		
	public void privateChain(boolean zClean) {
		//Set the Database backup manager
		getConsensusHandler().setBackUpManager();
		
		if(zClean){
			//Sort the genesis Block
			mConsensus.genesis();
		}
		
		//Tell miner we are auto mining..
		mTXMiner.setAutoMining(true);
		
		//No Hard Reset..
		mConsensus.setHardResetAllowed(false);
	}
	
	public void noChainReset() {
		//No Hard Reset..
		mConsensus.setHardResetAllowed(false);
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
			
			//Notify Listeners..
			mConsensus.updateListeners(new Message(ConsensusHandler.CONSENSUS_NOTIFY_QUIT));
			
			//Gracefull shutdown..
			mNetwork.PostMessage(NetworkHandler.NETWORK_SHUTDOWN);
			
			//Shut down the individual systems..
			mInput.stopMessageProcessor();
			mTXMiner.stopMessageProcessor();
			mConsensus.stopMessageProcessor();
			mBackup.stopMessageProcessor();
			
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
