
package org.minima.system;

import org.minima.objects.base.MiniNumber;
import org.minima.system.backup.BackupManager;
import org.minima.system.brains.ConsensusBackup;
import org.minima.system.brains.ConsensusHandler;
import org.minima.system.external.ProcessManager;
import org.minima.system.input.CommandFunction;
import org.minima.system.input.InputHandler;
import org.minima.system.network.NetworkHandler;
import org.minima.system.tx.TXMiner;
import org.minima.utils.MinimaLogger;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;

public class Main extends MessageProcessor {

	public static final String SYSTEM_STARTUP 		= "SYSTEM_STARTUP";
	
	public static final String SYSTEM_INIT 		    = "SYSTEM_INIT";
	
	public static final String SYSTEM_SHUTDOWN 		= "SYSTEM_SHUTDOWN";
	public static final String SYSTEM_FULLSHUTDOWN 	= "SYSTEM_FULLSHUTDOWN";
	
	public static final String SYSTEM_ALLSTOP 		= "SYSTEM_ALLSTOP";
	
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
	private TXMiner mTXMiner;
	
	/**
	 * The Main bottleneck thread that calculates the actual situation
	 */
	private ConsensusHandler mConsensus;
	
	/**
	 * The Backup Manager - runs in a separate thread
	 */
	private BackupManager mBackup;
	
	/**
	 * The Process manager that runs command line functions on receipt of transactions and blocks
	 */
	private ProcessManager mProcessManager;
	
	/**
	 * User Simulator.. for testing..
	 */
//	UserSimulator mSim;
	
	/**
	 * Are we creating a network from scratch
	 */
	boolean mGenesis = false;

	public int mPort;
	public int mRPCPort;
	
	public boolean mAutoConnect = false;
	public String mAutoHost 	= "";
	public int mAutoPort    	= 0;
	
	/**
	 * These values are filled by the Consensus for convenience..
	 * 
	 */
	MiniNumber mCurrentTopBlock;
	
	/**
	 * When did this node start up..
	 */
	long mNodeStartTime;
	
	/**
	 * Main COnstructor
	 * @param zPort
	 * @param zGenesis
	 */
	public Main(int zPort, int zRPCPort, boolean zGenesis, String zConfFolder) {
		super("MAIN     ");
		
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
		
		//Do it..
		mPort 		= zPort;
		mRPCPort	= zRPCPort;
		
		mInput 		= new InputHandler(this);
		
		mNetwork 	= new NetworkHandler(this);
		mTXMiner 	= new TXMiner(this);
		mBackup     = new BackupManager(this,zConfFolder);
		
		mProcessManager = new ProcessManager(this);
		
		//Create the Consensus handler..
		mConsensus  = new ConsensusHandler(this);
		
		mGenesis 	= zGenesis;
	}
	
	public void setAutoConnect(boolean zAuto) {
		mAutoConnect = zAuto;
	}
	
	public void setMiFiProxy(String zProxy){
		mNetwork.setProxy(zProxy);
	}
	
	public void setAutoConnectHostPort(String zHost, int zPort) {
		mAutoHost = zHost;
		mAutoPort = zPort;
	}
	
	public long getNodeStartTime() {
		return mNodeStartTime;
	}
	
	/**
	 * Do we run a command line function on receipt of a valid transaction
	 * Does not take depth nto account..
	 * @param zExec
	 */
	public void setNewTxnCommand(String zExec) {
		mProcessManager.setTXNCallFunction(zExec);
	}
	
	public void setNewRelCoin(String zPostURL) {
		mProcessManager.setRelCoin(zPostURL);
	}
	
	public void SystemShutDown() {
		PostMessage(SYSTEM_SHUTDOWN);
	}
		
	public void setTrace(boolean zTraceON) {
		setLOG(zTraceON);
		
		mConsensus.setLOG(zTraceON);
		mNetwork.PostMessage(new Message(NetworkHandler.NETWORK_TRACE).addBoolean("trace", zTraceON));
		mTXMiner.setLOG(zTraceON);
		mInput.setLOG(zTraceON);
		mBackup.setLOG(zTraceON);
		mProcessManager.setLOG(zTraceON);
		
//		mSim.setLOG(zTraceON);
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
	
	public ProcessManager getProcessManager() {
		return mProcessManager;
	}
	
	public TXMiner getMiner() {
		return mTXMiner;
	}
		
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		if ( zMessage.isMessageType(SYSTEM_STARTUP) ) {
			
			//Set the Database backup manager
			getConsensusHandler().setBackUpManager();
			
			//Are we genesis
			if(mGenesis) {
				//Sort the genesis Block
				mConsensus.genesis();
				
				//Tell miner we are auto mining..
				mTXMiner.setAutoMining(true);
				
				//No Hard Reset..
				mConsensus.setHardResetAllowed(false);
				
				//And init..
				PostMessage(SYSTEM_INIT);
				
			}else{
				//Restore..
				getConsensusHandler().PostMessage(ConsensusBackup.CONSENSUSBACKUP_RESTORE);
			}
			
		}else if ( zMessage.isMessageType(SYSTEM_INIT) ) {
			//Start the network..	
			Message netstart = new Message(NetworkHandler.NETWORK_STARTUP)
									.addInt("port", mPort)
									.addInt("rpcport", mRPCPort);
			mNetwork.PostMessage(netstart);

			//And do we do an automatic logon..
			if(mAutoConnect) {
				//Send a TimedMessage..
				Message connect  = new Message(NetworkHandler.NETWORK_CONNECT)
						.addInt("port", mAutoPort).addString("host", mAutoHost);
				
				getNetworkHandler().PostMessage(connect);
			}
			
		}else if ( zMessage.isMessageType(SYSTEM_SHUTDOWN) ) {
			//make a backup and shutdown message
			Message backshut = new Message(ConsensusBackup.CONSENSUSBACKUP_BACKUP);
			backshut.addBoolean("shutdown", true);
			
			//Save all the user details..
			getConsensusHandler().PostMessage(backshut);
			
		}else if ( zMessage.isMessageType(SYSTEM_FULLSHUTDOWN) ) {
			
			//Gracefull shutdown..
			mNetwork.PostMessage(NetworkHandler.NETWORK_SHUTDOWN);
			
			//Shut down the individual systems..
			mInput.stopMessageProcessor();
			mTXMiner.stopMessageProcessor();
			mConsensus.stopMessageProcessor();
			mBackup	.stopMessageProcessor();
			mProcessManager.stopMessageProcessor();
			
			//Wait..
			Thread.sleep(250);
			
			//And shut this down too..
			stopMessageProcessor();
			
			//Notify Listeners..
			mConsensus.updateListeners(new Message(ConsensusHandler.CONSENSUS_NOTIFY_QUIT));
			
			//All done..
			MinimaLogger.log("Minima Stopped. Bye Bye..");
			
		}else if ( zMessage.isMessageType(SYSTEM_ALLSTOP) ) {
			
			//Stop mining..
			String[] input= {"minetrans","off"};
			
			//Get the function..
			CommandFunction minetrans = CommandFunction.getFunction("minetrans");
			minetrans.setMainHandler(getConsensusHandler().getMainHandler());
			minetrans.doFunction(input);

			//Send.. 
			getNetworkHandler().PostMessage(new Message(NetworkHandler.NETWORK_ALLSTOP));
			
		}else {
			//Unknown Message..
			MinimaLogger.log("Unknown Message sent to main handler "+zMessage);
		}
	}

}
