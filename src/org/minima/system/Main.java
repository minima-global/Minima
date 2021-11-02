package org.minima.system;

import java.io.File;
import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.wallet.KeyRow;
import org.minima.objects.Pulse;
import org.minima.objects.TxBlock;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniNumber;
import org.minima.system.brains.TxPoWMiner;
import org.minima.system.brains.TxPoWProcessor;
import org.minima.system.genesis.GenesisMMR;
import org.minima.system.genesis.GenesisTxPoW;
import org.minima.system.network.NetworkManager;
import org.minima.system.network.minima.NIOManager;
import org.minima.system.network.minima.NIOMessage;
import org.minima.system.params.GeneralParams;
import org.minima.system.params.GlobalParams;
import org.minima.utils.MiniFile;
import org.minima.utils.MinimaLogger;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;
import org.minima.utils.messages.TimerProcessor;

public class Main extends MessageProcessor {

	/**
	 * Static link to the MAIN class
	 */
	private static Main mMainInstance = null;
	public static Main getInstance() {
		return mMainInstance;
	}
	
	public static final String MAIN_TXPOWMINED 	= "MAIN_TXPOWMINED";
	public static final String MAIN_AUTOMINE 	= "MAIN_AUTOMINE";
	public static final String MAIN_CLEANDB 	= "MAIN_CLEANDB";
	public static final String MAIN_PULSE 		= "MAIN_PULSE";
	
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
	 * Are we shutting down..
	 */
	boolean mShuttingdown = false;
	
	/**
	 * Timer delay for CleanDB messages - every 30 mins
	 */
	long CLEANDB_TIMER	= 1000 * 60 * 30;
	
	public Main() {
		super("MAIN");
	
		mMainInstance = this;
	
		//Are we deleting previous..
		if(GeneralParams.CLEAN) {
			MinimaLogger.log("Wiping previous config files..");
			//Delete the conf folder
			MiniFile.deleteFileOrFolder(GeneralParams.CONFIGURATION_FOLDER, new File(GeneralParams.CONFIGURATION_FOLDER));
		}
		
		//Create the MinmaDB
		MinimaDB.createDB();
		
		//Load the Databases
		MinimaDB.getDB().loadAllDB();
		
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
		
		//Simulate traffic message ( only if auto mine is set )
		PostTimerMessage(new TimerMessage(MiniNumber.THOUSAND.div(GlobalParams.MINIMA_BLOCK_SPEED).getAsLong(), MAIN_AUTOMINE));
		
		//Set the PULSE message timer.
		PostTimerMessage(new TimerMessage(GeneralParams.USER_PULSE_FREQ, MAIN_PULSE));
	}
	
	public void shutdown() {
		//we are shutting down
		mShuttingdown = true;
		
		//Shut down the network
		mNetwork.shutdownNetwork();
				
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
		
		//Stop this..
		stopMessageProcessor();
				
		//Now backup the  databases
		MinimaDB.getDB().saveAllDB();
		
		//Wait for it..
		while(!isShutdownComplete()) {
			try {Thread.sleep(50);} catch (InterruptedException e) {}
		}		
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
	
	public void setTrace(boolean zTrace) {
		setFullLogging(zTrace);
		mTxPoWProcessor.setFullLogging(zTrace);
		mTxPoWMiner.setFullLogging(zTrace);
		mNetwork.getNIOManager().setFullLogging(zTrace);
		mNetwork.getP2PManager().setFullLogging(zTrace);
	}
	
	private void doGenesis() {
		
		//Create a new key - to receive the genesis funds..
		KeyRow genkey = MinimaDB.getDB().getWallet().createNewKey();
		
		//Create the Genesis TxPoW..
		GenesisTxPoW genesis = new GenesisTxPoW(genkey.getAddress());
		
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
		if(mShuttingdown) {
			return;
		}
		
		//Process messages
		if(zMessage.getMessageType().equals(MAIN_TXPOWMINED)) {
			//Get it..
			TxPoW txpow = (TxPoW) zMessage.getObject("txpow");
			
			//We have mined a TxPoW.. send it out to the network..
			if(!txpow.isTransaction() && !txpow.isBlock()) {
				//A PULSE..forward as proof
				return;
			}
			
//			//Did we find a block 
//			if(txpow.isBlock()) {
//				MinimaLogger.log("You found a block! "+txpow.getTxPoWID() );
//				
//				//Check it..
//				TxPoWChecker.checkTxPoW(MinimaDB.getDB().getTxPoWTree().getTip().getMMR(), txpow);
//			}
			
			//New TxPoW!.. add to database and send on to the Processor
			mTxPoWProcessor.postProcessTxPoW(txpow);
			
			//FOR NOW.. ( Should just send the full TxPoW - we just mined it so noone has it)
			NIOManager.sendNetworkMessageAll(NIOMessage.MSG_TXPOWID, txpow.getTxPoWIDData());
		
		}else if(zMessage.getMessageType().equals(MAIN_AUTOMINE)) {
			
			//Are we auto mining
			if(GeneralParams.AUTOMINE) {
				
				//Create a TxPoW
				mTxPoWMiner.PostMessage(TxPoWMiner.TXPOWMINER_EMPTYTXPOW);
			}
			
			//Next Attempt
			PostTimerMessage(new TimerMessage(MiniNumber.THOUSAND.div(GlobalParams.MINIMA_BLOCK_SPEED).getAsLong(), MAIN_AUTOMINE));
		
		}else if(zMessage.getMessageType().equals(MAIN_CLEANDB)) {
			
			//Do some house keeping on the DB
			MinimaDB.getDB().getTxPoWDB().cleanDB();
			
			//Same with the ArchiveDB
			MinimaDB.getDB().getArchive().cleanDB();
			
			//Clean up the RAM Memory
			System.gc();
			
			//Do it again..
			PostTimerMessage(new TimerMessage(CLEANDB_TIMER, MAIN_CLEANDB));
		
		}else if(zMessage.getMessageType().equals(MAIN_PULSE)) {
			
			//Create Pulse Message
			Pulse pulse = Pulse.createPulse();
		
			//And send it to all your peers..
			NIOManager.sendNetworkMessageAll(NIOMessage.MSG_PULSE, pulse);
		
			//Mine a TxPoW
			mTxPoWMiner.PostMessage(TxPoWMiner.TXPOWMINER_EMPTYTXPOW);
			
			//And then wait again..
			PostTimerMessage(new TimerMessage(GeneralParams.USER_PULSE_FREQ, MAIN_PULSE));
		}
	}
}
