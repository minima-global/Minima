package org.minima.system.brains;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Hashtable;
import java.util.Random;

import org.minima.GlobalParams;
import org.minima.database.MinimaDB;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.StateVariable;
import org.minima.objects.Transaction;
import org.minima.objects.TxPoW;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.objects.proofs.TokenProof;
import org.minima.system.Main;
import org.minima.system.input.InputHandler;
import org.minima.system.input.functions.gimme50;
import org.minima.system.input.functions.newaddress;
import org.minima.system.network.NetworkHandler;
import org.minima.system.network.base.MinimaClient;
import org.minima.system.network.base.MinimaReader;
import org.minima.system.network.minidapps.DAPPManager;
import org.minima.system.txpow.TxPoWChecker;
import org.minima.system.txpow.TxPoWMiner;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageListener;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;

public class ConsensusHandler extends MessageProcessor {

	/**
	 * Main processing loop for a txpow message
	 */
	public static final String CONSENSUS_PROCESSTXPOW 		   = "CONSENSUS_PROCESSTXPOW";
	
	/**
	 * Auto backup every 10 minutes..
	 */
	public static final String CONSENSUS_AUTOBACKUP 	       = "CONSENSUS_AUTOBACKUP";
	public static final long AUTOBACKUP_TIMER 				   = 1000 * 60 * 30;
	
	/**
	 * Flush the mempool..
	 */
	public static final String CONSENSUS_FLUSH 	       		   = "CONSENSUS_FLUSH";
	public static final long FLUSH_TIMER 				   	   = 1000 * 60 * 10;
	
	/**
	 * Reconnect if need be - no tip change for 5 mins..
	 */
	public static final String CONSENSUS_CHECK_RECONNECT 	   = "CONSENSUS_CHECK_RECONNECT";
	public static final long CHECK_RECONNECT_TIMER 			   = 1000 * 60 * 10;
	boolean mFirstReconnectRun 	= true;
	MiniNumber mLastTip 		= MiniNumber.ZERO;
	
	/**
	 * Auto Consolidate 
	 */
	public static final String CONSENSUS_AUTOCONSOLIDATE 	   = "CONSENSUS_AUTOCONSOLIDATE";
	public static final long CONSOLIDATE_TIMER 				   = 1000 * 60 * 60;
	
	/**
	 * Initialise the 32 keys you use by default for change
	 * This can be slow at time of send so best to do it incrementally..
	 */
	public static final String CONSENSUS_INITKEYS 	       		= "CONSENSUS_INITKEYS";
	public static final long INITKEYS_TIMER 				    = 1000 * 60;
	
	/**
	 * HARD CORE MINIMG for the bootstrap period 
	 */
	public static final String CONSENSUS_ACTIVATEMINE 		   = "CONSENSUS_ACTIVATEMINE";
	public static final String CONSENSUS_MINEBLOCK 			   = "CONSENSUS_MINEBLOCK";
	public static final String CONSENSUS_DEBUGMINE 			   = "CONSENSUS_DEBUGMINE";
	
	/**
	 * TestNET 
	 */
	public static final String CONSENSUS_GIMME50 			   = "CONSENSUS_GIMME50";
	
	/**
	 * Transaction functions
	 */
	public static final String CONSENSUS_CREATETRANS 		   = "CONSENSUS_CREATETRANS";
	public static final String CONSENSUS_SENDTRANS 			   = "CONSENSUS_SENDTRANS";
	public static final String CONSENSUS_FINISHED_MINE 		   = "CONSENSUS_FINISHED_MINE";
	
	/**
	 * PULSE
	 */
	public static final String CONSENSUS_PULSE_START 			= "CONSENSUS_PULSE_START";
	public static final String CONSENSUS_PULSE 			   		= "CONSENSUS_PULSE";
	public static final String CONSENSUS_PULSE_MINED 			= "CONSENSUS_PULSE_MINED";
	public static final long PULSE_TIMER = 1000 * 60 * 10;
	
	/**
	 * Create Tokens
	 */
	public static final String CONSENSUS_TOKENCREATE 		= "CONSENSUS_TOKENCREATE";
	
	/**
	 * Notification Messages
	 */
	public static final String CONSENSUS_NOTIFY_QUIT 	    = "CONSENSUS_NOTIFY_QUIT";
	public static final String CONSENSUS_NOTIFY_BALANCE 	= "CONSENSUS_NOTIFY_BALANCE";
	public static final String CONSENSUS_NOTIFY_NEWBLOCK 	= "CONSENSUS_NOTIFY_NEWBLOCK";
	public static final String CONSENSUS_NOTIFY_ACTION 	    = "CONSENSUS_NOTIFY_ACTION";
	public static final String CONSENSUS_NOTIFY_MINESTART 	= "CONSENSUS_NOTIFY_MINESTART";
	public static final String CONSENSUS_NOTIFY_MINEEND 	= "CONSENSUS_NOTIFY_MINEEND";
	public static final String CONSENSUS_NOTIFY_INITIALSYNC = "CONSENSUS_NOTIFY_INITIALSYNC";
	public static final String CONSENSUS_NOTIFY_INITIALPERC = "CONSENSUS_NOTIFY_INITIALPERC";
	public static final String CONSENSUS_NOTIFY_LOG         = "CONSENSUS_NOTIFY_LOG";
	public static final String CONSENSUS_NOTIFY_RECONNECT   = "CONSENSUS_NOTIFY_RECONNECT";
	
	public static final String CONSENSUS_NOTIFY_DAPP_RELOAD    = "CONSENSUS_NOTIFY_DAPP_RELOAD";
	public static final String CONSENSUS_NOTIFY_DAPP_INSTALLED = "CONSENSUS_NOTIFY_DAPP_INSTALLED";
		
	/**
	 * Print the chain whenever you get a new block ?
	 */
	public boolean mPrintChain = false;
	
	/**
	 * The main database. It is ONLY accessed from this single thread
	 */
	MinimaDB mMainDB;
	
	/**
	 * User transaction handler
	 */
	ConsensusTxn mConsensusTxn;
	
	/**
	 * Network message handler
	 */
	ConsensusNet mConsensusNet;
	
	/**
	 * User messages
	 */
	ConsensusUser mConsensusUser;
	
	/**
	 * Print Functions that access the main DB
	 */
	ConsensusPrint mConsensusPrint;
	
	/**
	 * The backup restore manager
	 */
	ConsensusBackup mConsensusBackup;
	
	/**
	 * A list of Listeners.. for important messages..
	 */
	ArrayList<MessageListener> mListeners;
	
	/**
	 * FLUSH counter 
	 */
	int mFlushCounter = 0;
	
	/**
	 * The Last Gimme50..
	 */
	long mLastGimme = 0;
	public static final long MIN_GIMME50_TIME_GAP = 1000 * 60 * 60 * 24;
	
	/**
	 * PULSE Timer - every 10 mins
	 */
	
	
	/**
	 * Consolidate Time - Every  hour
	 */
	
	
	/**	
	 * Main Constructor
	 * @param zMain
	 */
	public ConsensusHandler() {
		super("CONSENSUS");
		
		//Create a database..
		mMainDB = new MinimaDB();
		
		//The Listeners
		mListeners = new ArrayList<>();
		
		//Handles custom Txn messages
		mConsensusTxn    = new ConsensusTxn(mMainDB, this);
		mConsensusNet    = new ConsensusNet(mMainDB, this);
		mConsensusUser   = new ConsensusUser(mMainDB, this);
		mConsensusPrint  = new ConsensusPrint(mMainDB, this);
		mConsensusBackup = new ConsensusBackup(mMainDB, this);
		
		//Are we HARD mining.. debugging / private chain
		PostTimerMessage(new TimerMessage(5000, CONSENSUS_MINEBLOCK));
	
		//Redo every 10 minutes..
		PostTimerMessage(new TimerMessage(AUTOBACKUP_TIMER, CONSENSUS_AUTOBACKUP));
		
		//Flush Mempool - 5 min delay
		PostTimerMessage(new TimerMessage(FLUSH_TIMER + (1000 * 60 * 5), CONSENSUS_FLUSH));
		
		//Initialise the multi keys..
		PostTimerMessage(new TimerMessage(INITKEYS_TIMER, CONSENSUS_INITKEYS));
		
		//Start the PULSE - every 10 minutes
		PostTimerMessage(new TimerMessage(PULSE_TIMER, CONSENSUS_PULSE_START));
		
		//Auto Consolidate - every hour
		PostTimerMessage(new TimerMessage(CONSOLIDATE_TIMER, CONSENSUS_AUTOCONSOLIDATE));
	
		//Re-check 
		PostTimerMessage(new TimerMessage(CHECK_RECONNECT_TIMER, CONSENSUS_CHECK_RECONNECT));
	}
	
	public void setBackUpManager() {
		getMainDB().setBackupManager(Main.getMainHandler().getBackupManager());
	}
	
	/**
	 * Get the ConsensusNet Handler
	 */
	public ConsensusNet getConsensusNet() {
		return mConsensusNet;
	}
	
	/**
	 * Listener Functions
	 */
	public void clearListeners() {
		mListeners.clear();
	}
	
	public void addListener(MessageListener zListen) {
		mListeners.add(zListen);
	}
	
	public void removeListener(MessageListener zListen) {
		mListeners.remove(zListen);
	}
	
	public void updateListeners(Message zMessage) {
		for(MessageListener listen : mListeners) {
			listen.processMessage(zMessage);
		}
	}
	
	public void notifyInitialListeners(String zMessage) {
		updateListeners(new Message(ConsensusHandler.CONSENSUS_NOTIFY_INITIALPERC).addString("info", zMessage));
	}
	
	/**
	 * Are we initialising the Genesis block
	 */
	public void genesis() {
		getMainDB().DoGenesis();
	}
	
	private MinimaDB getMainDB() {
		return mMainDB;
	}
	
	public void setInitialSyncComplete() {
		mConsensusNet.setInitialSyncComplete();
	}
	
	public boolean isInitialSyncComplete() {
		return mConsensusNet.isInitialSyncComplete();
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		/**
		 * Main processing function.. 
		 */
		if ( zMessage.isMessageType(CONSENSUS_PROCESSTXPOW) ) {
			//A TXPOW - that has been checked already and added to the DB
			TxPoW txpow = (TxPoW) zMessage.getObject("txpow");
			
			//ADD TO THE DATABASE..
			getMainDB().addNewTxPow(txpow);
			
			//Check the validity..
			String txpowid = txpow.getTxPowID().to0xString();
			
			//Was it requested (could be from a branch) ?
			NetworkHandler network =  Main.getMainHandler().getNetworkHandler();
			boolean requested = false;
			if(network.isRequestedTxPow(txpowid)) {
				requested = true;
				network.removeRequestedTxPow(txpowid);
			}
			
			//Is this a syncmessage
			boolean syncmessage = zMessage.getBoolean("sync");
			
			//Check the Validity..
			boolean txnok = TxPoWChecker.checkTransactionMMR(txpow, getMainDB());
			if(!txnok) {
				MinimaLogger.log("WARNING Invalid TXPOW (Requested.. "+requested+" ) trans:"+txpow.isTransaction()+" blk:"+txpow.isBlock()+" " +txpow.getBlockNumber()+" "+txpow.getTxPowID()+" tip:"+getMainDB().getTopBlock());
				if(!requested) {
					//What to do.. could be in a spearate branch.. but if unrequested.. 
					//..	
				}
			}
			
			//Is it relevant to you the User
			boolean relevant = false;
			if(txpow.isTransaction()) {
				//Is it relevant to us..
				relevant = getMainDB().getUserDB().isTransactionRelevant(txpow.getTransaction());
			}
			
			//Send a message to all about a new TxPoW (may or may not be a transaction or a block..)
			JSONObject newtxpow = new JSONObject();
			newtxpow.put("event","newtxpow");
			newtxpow.put("txpow",txpow.toJSON());
			newtxpow.put("relevant",relevant);
			PostDAPPJSONMessage(newtxpow);
			
			//Back it up!
			Main.getMainHandler().getBackupManager().backupTxpow(txpow);
			
			//What's the current chain tip..
			MiniData oldtip = getMainDB().getMainTree().getChainTip().getTxPowID();
			
			//Process
			getMainDB().processTxPOW(txpow);
		
			//What's the new chain tip..
			TxPoW newtip = getMainDB().getMainTree().getChainTip().getTxPow();
			
			//Has there been a change
			boolean newbalance = false;
			if(!oldtip.isEqual(newtip.getTxPowID())) {
				newbalance = true;
				
				//Notify..
				updateListeners(new Message(CONSENSUS_NOTIFY_NEWBLOCK, zMessage).addObject("txpow", newtip));
			
				//Update the web listeners..
				JSONObject newblock = new JSONObject();
				newblock.put("event","newblock");
				newblock.put("txpow",newtip.toJSON());
				PostDAPPJSONMessage(newblock);
				
				//Do the balance.. Update listeners if changed..
				if(!syncmessage) {
					PostMessage(new Message(ConsensusPrint.CONSENSUS_BALANCE, zMessage).addBoolean("hard", true));
				
					//Print the tree..
					if(mPrintChain) {
						PostMessage(new Message(ConsensusPrint.CONSENSUS_PRINTCHAIN_TREE, zMessage).addBoolean("systemout", true));
					}
				}
			}
			
			//If it's relevant to the user..
			if(relevant) {
				//Get the Token Amounts..
				Hashtable<String, MiniNumber> tokamt = getMainDB().getTransactionTokenAmounts(txpow);
				
				//Store ion the database..
				getMainDB().getUserDB().addToHistory(txpow.deepCopy(),tokamt);
				
				//Do we need to update the balance.. or did we do it already..
				updateListeners(new Message(CONSENSUS_NOTIFY_BALANCE, zMessage));
				if(!newbalance && !syncmessage) {
					PostMessage(new Message(ConsensusPrint.CONSENSUS_BALANCE, zMessage).addBoolean("hard", true));
				}				
			}
					
			//BROADCAST Message for ALL the clients - only if valid / or block.. ( they can request it if need be..)
			if(!syncmessage && (txnok || txpow.isBlock())) {
				Message netmsg  = new Message(MinimaClient.NETCLIENT_SENDTXPOWID, zMessage).addObject("txpowid", txpow.getTxPowID());
				Message netw    = new Message(NetworkHandler.NETWORK_SENDALL, zMessage).addObject("message", netmsg);
				Main.getMainHandler().getNetworkHandler().PostMessage(netw);
			}
			
			//Remove from the List of Mined transactions.. ( probably not ours but good to do it here )
			getMainDB().remeoveMiningTransaction(txpow.getTransaction());
				
	
			//AUTO Messages
		}else if ( zMessage.isMessageType(CONSENSUS_FLUSH) ) {
			
			//Clear the current Requested Transactions.. this should ask for them all anyway..
			Main.getMainHandler().getNetworkHandler().clearAllrequestedTxPow();
			
			//Post a FULL resync message
			PostMessage(new Message(ConsensusNet.CONSENSUS_NET_FULLTREERESYSNC, zMessage));
			
			//Flush / Check the mem-pool
//			PostMessage(new Message(ConsensusUser.CONSENSUS_FLUSHMEMPOOL));
			
			//Clean the Tokens..
			getMainDB().checkTokens();
			
			//Re-do
			PostTimerMessage(new TimerMessage(FLUSH_TIMER, CONSENSUS_FLUSH));
			
			//Clean the Memory..
			System.gc();
		
		}else if ( zMessage.isMessageType(CONSENSUS_CHECK_RECONNECT) ) {
			//Check ready
			if(getMainDB().getMainTree().getChainTip() == null) {
				//Wait a bit..
				PostTimerMessage(new TimerMessage(CHECK_RECONNECT_TIMER, CONSENSUS_CHECK_RECONNECT));
				return;
			}
			
			//Current tip
			MiniNumber currenttip = getMainDB().getTopBlock();
			
			//Is it the first time 
			if(mFirstReconnectRun) {
//				MinimaLogger.log("FIRST RECONNECT @ "+currenttip);
				
				mLastTip 			= currenttip;
				mFirstReconnectRun 	= false;
			
			}else{
				//Check if there is a change..
				if(mLastTip.isEqual(currenttip)) {
					MinimaLogger.log("RECONNECT after no tip change! @ "+mLastTip);
					
					//Same block number after 5 mins ? reconnect and resync..
					Message reconnect = new Message(NetworkHandler.NETWORK_RECONNECT, zMessage);
					Main.getMainHandler().getNetworkHandler().PostMessage(reconnect);
					
					mFirstReconnectRun = true;
				}else {
//					MinimaLogger.log("TIP CHANGED RECONNECT @ "+mLastTip+" / "+currenttip);
				}
				
				//And check for next time
				mLastTip = currenttip;
			}
			
			//Re-check 
			PostTimerMessage(new TimerMessage(CHECK_RECONNECT_TIMER, CONSENSUS_CHECK_RECONNECT));
			
		}else if ( zMessage.isMessageType(CONSENSUS_AUTOBACKUP) ) {
			//Backup the system..
			PostMessage(ConsensusBackup.CONSENSUSBACKUP_BACKUP);
			
			//Redo every 10 minutes..
			PostTimerMessage(new TimerMessage(AUTOBACKUP_TIMER, CONSENSUS_AUTOBACKUP));
			
		}else if ( zMessage.isMessageType(CONSENSUS_AUTOCONSOLIDATE) ) {
			//Consolidate your coins! - default is FALSE
			if(Main.getMainHandler().getUserPrefs().getBoolean("consolidate", false)) {
				PostMessage(new Message(ConsensusUser.CONSENSUS_CONSOLIDATE, zMessage));
			}
		
			//Auto Consolidate - every hour
			PostTimerMessage(new TimerMessage(CONSOLIDATE_TIMER, CONSENSUS_AUTOCONSOLIDATE));
			
			/**
			 * Initialise the Multi Keys..
			 */
		}else if ( zMessage.isMessageType(CONSENSUS_INITKEYS) ) {
			//Check keys..
			boolean inited = getMainDB().getUserDB().checkInitKeys(this);
			
			//Do it again..
			if(inited) {
				PostTimerMessage(new TimerMessage(INITKEYS_TIMER, CONSENSUS_INITKEYS));
			}
			
			System.gc();
			
		/**
		 * Network Messages
		 */
		}else if(zMessage.getMessageType().startsWith(ConsensusNet.CONSENSUS_PREFIX)) {
			mConsensusNet.processMessage(zMessage);
			
		/**
		 * Custom Transactions
		 */
		}else if(zMessage.getMessageType().startsWith(ConsensusTxn.CONSENSUS_PREFIX)) {
			mConsensusTxn.processMessage(zMessage);
		
		/**
		 * User Messages
		 */
		}else if(zMessage.getMessageType().startsWith(ConsensusUser.CONSENSUS_PREFIX)) {
			mConsensusUser.processMessage(zMessage);
		
		/**
		 * Print Messages
		 */
		}else if(zMessage.getMessageType().startsWith(ConsensusPrint.CONSENSUS_PREFIX)) {
			mConsensusPrint.processMessage(zMessage);
		
		/**
		 * Backup Messages
		 */
		}else if(zMessage.getMessageType().startsWith(ConsensusBackup.CONSENSUS_PREFIX)) {
			mConsensusBackup.processMessage(zMessage);
		
		/**
		 *  Are we HARD mining
		 */
		}else if ( zMessage.isMessageType(CONSENSUS_ACTIVATEMINE) ) {
			boolean mining = zMessage.getBoolean("automining");
			Main.getMainHandler().getMiner().setAutoMining(mining);
			
			JSONObject resp = InputHandler.getResponseJSON(zMessage);
			resp.put("automining", mining);			
			InputHandler.endResponse(zMessage, true, "");
		
			//Boot-Strap Mining  
		}else if ( zMessage.isMessageType(CONSENSUS_MINEBLOCK) ) {
			//DEBUG MODE - only mine a block when you make a transction..
			if(GlobalParams.MINIMA_ZERO_DIFF_BLK) {return;}
			
			//Are we Mining..
			if(!isInitialSyncComplete() || !Main.getMainHandler().getMiner().isAutoMining()) {
				PostTimerMessage(new TimerMessage(20000, CONSENSUS_MINEBLOCK));
				return;
			}
			
			//Fresh TXPOW
			TxPoW txpow = getMainDB().getCurrentTxPow(new Transaction(), new Witness(), new JSONArray());
			
			//Do we have any blocks.. could be syncing
			if(txpow == null) {
				//Try again in a minute
				PostTimerMessage(new TimerMessage(20000, CONSENSUS_MINEBLOCK));
				return;
			}
			
			//Send it to the Miner..
			Message mine = new Message(TxPoWMiner.TXMINER_MEGAMINER, zMessage).addObject("txpow", txpow);
			
			//Post to the Miner
			Main.getMainHandler().getMiner().PostMessage(mine);
		
		}else if ( zMessage.isMessageType(CONSENSUS_DEBUGMINE) ) {
			//Mine one single block.. 
			TxPoW txpow = getMainDB().getCurrentTxPow(new Transaction(), new Witness(), new JSONArray());
			
			//Send it to the Miner..
			Message mine = new Message(TxPoWMiner.TXMINER_DEBUGBLOCK, zMessage).addObject("txpow", txpow);
			
			//Continue the log output trail
			InputHandler.addResponseMesage(mine, zMessage);
			
			//Post to the Miner
			Main.getMainHandler().getMiner().PostMessage(mine);
			
		/**
		 * Transaction management
		 */
		}else if ( zMessage.isMessageType(CONSENSUS_SENDTRANS) ) {
			//get The Transaction
			Transaction trans = (Transaction) zMessage.getObject("transaction");
		
			//Get the Witness data if a valid transaction and not just an off chain zero transaction
			Witness wit = (Witness) zMessage.getObject("witness");
			
			JSONObject resp = InputHandler.getResponseJSON(zMessage);
			
			//Add it to the current TX-POW
			JSONArray contractlogs = new JSONArray();
			TxPoW txpow = getMainDB().getCurrentTxPow(trans, wit, contractlogs);
			
			//Is is valid.. ?
			if(txpow==null) {
				//Remove from the List of Mined transactions..
				getMainDB().remeoveMiningTransaction(trans);
				
				//Notify listeners that Mining is starting...
				PostDAPPEndMining(trans);
				
				resp.put("contractlogs", contractlogs);
				InputHandler.endResponse(zMessage, false, "Invalid Transaction");
				return;
			}
			
			//Create the correct transID..
			txpow.setHeaderBodyHash();
			txpow.calculateTXPOWID();
			
			//Check the SIGS!
			boolean sigsok = TxPoWChecker.checkSigs(txpow);
			if(!sigsok) {
				//Remove from the List of Mined transactions..
				getMainDB().remeoveMiningTransaction(txpow.getTransaction());
				
				//Notify listeners that Mining is starting...
				PostDAPPEndMining(txpow.getTransaction());
				
				//Reject
				InputHandler.endResponse(zMessage, false, "Invalid Signatures! - TXNAUTO must be done AFTER adding state variables ?");
				return;
			}
			
			//Final check of the mempool coins..
			if(getMainDB().checkTransactionForMempoolCoins(trans)) {
				//Remove from the List of Mined transactions..
				getMainDB().remeoveMiningTransaction(txpow.getTransaction());
				
				//Notify listeners that Mining is starting...
				PostDAPPEndMining(txpow.getTransaction());
				
				//No GOOD!
				InputHandler.endResponse(zMessage, false, "ERROR double spend coin in mempool.");
				return;
			}
			
			//Add the size..
			resp.put("size", txpow.getSizeinBytes());
			resp.put("inputs", txpow.getTransaction().getAllInputs().size());
			resp.put("outputs", txpow.getTransaction().getAllOutputs().size());
			
			if(txpow.getSizeinBytes() > MinimaReader.MAX_TXPOW) {
				//Remove from the List of Mined transactions..
				getMainDB().remeoveMiningTransaction(txpow.getTransaction());
				
				//Notify listeners that Mining is starting...
				PostDAPPEndMining(txpow.getTransaction());
				
				//Add the TxPoW
				resp.put("transaction", txpow.getTransaction());
				
				//ITS TOO BIG!
				MinimaLogger.log("Transaction TOO big! "+txpow.getSizeinBytes());
				InputHandler.endResponse(zMessage, false, "YOUR TXPOW TRANSACTION IS TOO BIG! MAX SIZE : "+MinimaReader.MAX_TXPOW);
				
				return;
			}
			
			//Send it to the Miner.. This is the ONLY place this happens..
			Message mine = new Message(TxPoWMiner.TXMINER_MINETXPOW, zMessage).addObject("txpow", txpow);
			Main.getMainHandler().getMiner().PostMessage(mine);
		
			//Add the TxPoW
			resp.put("txpow", txpow);
			
			InputHandler.endResponse(zMessage, true, "Send Success");
			
		}else if ( zMessage.isMessageType(CONSENSUS_CREATETRANS) ) {
			//How much to who ?
			String address 	= zMessage.getString("address");
			if(address.startsWith("0x")) {
				//It's a regular HASH address
				address = new MiniData(address).to0xString();
			}else if(address.startsWith("Mx")) {
				//It's a Minima Address!
				address = Address.convertMinimaAddress(address).to0xString();
			}
			
			String tokenid 	   	= new MiniData(zMessage.getString("tokenid")).to0xString();
			String amount  		= zMessage.getString("amount");
			String state  		= zMessage.getString("state");
			
			//The Token Hash
			MiniData tok       		= new MiniData(tokenid);
			MiniData changetok 		= new MiniData(tokenid);
			
			//Replace with the HASH value.. 
			tokenid = tok.to0xString();
			
			//Is this a token amount or a minima amount
			TokenProof tokendets = null;
			if(!tok.isEqual(Coin.MINIMA_TOKENID)) {
				//It's a token.. scale it..
				MiniNumber samount = new MiniNumber(amount);
				
				//Now divide by the scale factor..
				tokendets = getMainDB().getUserDB().getTokenDetail(new MiniData(tokenid));
				
				//Do we have it,.
				if(tokendets == null) {
					//Unknown token!
					InputHandler.endResponse(zMessage, false, "No details found for the specified token : "+tokenid);
					return;
				}
				
				//Scale..
				samount = tokendets.getScaledMinimaAmount(samount);
//				samount = samount.div(tokendets.getScaleFactor());
				
				//And set the new value..
				amount = samount.toString();
			}
			
			//Send details..
			MiniNumber sendamount 	= new MiniNumber(amount);
			
			//Check is a valid amount.
			if(!sendamount.isValidMinimaValue() || !sendamount.isMore(MiniNumber.ZERO)) {
				InputHandler.endResponse(zMessage, false, "Invalid amount specified for send amount! "+sendamount);
				return;
			}
			
			//How much do we have..
			ArrayList<Coin> confirmed = null;
			if(tok.isEqual(Coin.TOKENID_CREATE)) {
				confirmed = getMainDB().getTotalSimpleSpendableCoins(Coin.MINIMA_TOKENID);
				changetok = Coin.MINIMA_TOKENID;
			}else {
				confirmed = getMainDB().getTotalSimpleSpendableCoins(tok);
			}
			
			//Select the coins to use in the transaction
			ArrayList<Coin> selectedCoins = selectCoins(confirmed, sendamount);
		
			//Do we have enough funds..
			if(selectedCoins.size()==0) {
				//Sum the confirmed coins..
				MiniNumber conftotal = new MiniNumber();
				for(Coin cc : confirmed) {
					conftotal = conftotal.add(cc.getAmount());
				}
				
				//Insufficient funds!
				if(!tokenid.equals(Coin.MINIMA_TOKENID.to0xString())) {
					conftotal = tokendets.getScaledTokenAmount(conftotal);
					InputHandler.endResponse(zMessage, false, "Insufficient funds! You only have : "+conftotal);
				}else {
					InputHandler.endResponse(zMessage, false, "Insufficient funds! You only have : "+conftotal);
				}
				
				return;
			}

			//What are we sending
			MiniNumber total = new MiniNumber(); 
			for(Coin cc : selectedCoins) {
				total = total.add(cc.getAmount());
			}
			
			//Continue constructing the transaction - outputs don't need scripts
			Address recipient= new Address(new MiniData(address));
			
			//Blank address - check change is non-null
			Address change = new Address(); 
			if(!total.isEqual(sendamount)) {
				change = getMainDB().getUserDB().getCurrentAddress(this);
			}
			
			//Create the Transaction
			Message ret = getMainDB().createTransaction(sendamount, recipient, change, 
					selectedCoins, tok, changetok, null, new Transaction(), state, true);
			
			//Is this a token transaction
			if(tokendets != null) {
				//Get the witness and add relevant info..
				Witness wit = (Witness) ret.getObject("witness");
				
				//Get the token details..
				wit.addTokenDetails(tokendets);
			}
			
			//get the Transaction
			Transaction trans = (Transaction) ret.getObject("transaction");
			
			//Final check..
			if(getMainDB().checkTransactionForMining(trans)) {
				InputHandler.endResponse(zMessage, false, "ERROR double spend coin in mining pool.");
				return;
			}
			
			//Add all the inputs to the mining..
			getMainDB().addMiningTransaction(trans);
			
			//Notify listeners that Mining is starting...
			PostDAPPStartMining(trans);
			
			//Get the message ready
			InputHandler.addResponseMesage(ret, zMessage);
			
			//Send it..
			PostMessage(ret);
		
		}else if ( zMessage.isMessageType(CONSENSUS_PULSE_START) ) {
			//Post a PULSE message
			PostMessage(CONSENSUS_PULSE);
		
//			//Start again in 10 minutes..
//			PostTimerMessage(new TimerMessage(PULSE_TIMER, CONSENSUS_PULSE_START));
			
		}else if ( zMessage.isMessageType(CONSENSUS_PULSE) ) {
			MinimaLogger.log("PULSE MINE RUNNING..");
			
			//PULSE Txn 
			TxPoW txpow = getMainDB().getCurrentTxPow(new Transaction(), new Witness(), new JSONArray());
			
			//Send it to the Miner..
			Message mine = new Message(TxPoWMiner.TXMINER_PULSE, zMessage).addObject("txpow", txpow);
			
			//Post to the Miner
			Main.getMainHandler().getMiner().PostMessage(mine);
			
		}else if ( zMessage.isMessageType(CONSENSUS_PULSE_MINED) ) {
			//The TXPOW
			TxPoW txpow = (TxPoW) zMessage.getObject("txpow");

			//Is it a block ? - otherwise do nothing..
			if(txpow.isBlock()) {
				//Post onwards!
				MinimaLogger.log("Congratulations! You found a PULSE block @ "+txpow.getBlockNumber());
				
				//And now forward the message to the single entry point..
				Message msg = new Message(ConsensusNet.CONSENSUS_NET_CHECKSIZE_TXPOW, zMessage).addObject("txpow", txpow);
				PostMessage(msg);
			}else {
//				MinimaLogger.log("PULSE Finished @ "+txpow.getBlockNumber());
			}
			
			//Start again in 10 minutes..
			MinimaLogger.log("PULSE MINE FINISHED.. wait 10 minutes to restart");
			PostTimerMessage(new TimerMessage(PULSE_TIMER, CONSENSUS_PULSE));
			
		}else if(zMessage.isMessageType(CONSENSUS_FINISHED_MINE)) {
			//The TXPOW
			TxPoW txpow = (TxPoW) zMessage.getObject("txpow");
			
			//And now forward the message to the single entry point..
			Message msg = new Message(ConsensusNet.CONSENSUS_NET_CHECKSIZE_TXPOW, zMessage).addObject("txpow", txpow);
			PostMessage(msg);
			
			//Notify listeners that Mining is starting...
			PostDAPPEndMining(txpow.getTransaction());
			
		}else if(zMessage.isMessageType(CONSENSUS_GIMME50)) {
			//Check time
			long timenow = System.currentTimeMillis();
			if(timenow - mLastGimme < MIN_GIMME50_TIME_GAP) {
				//You can only do one of these every 24 hours..
				InputHandler.endResponse(zMessage, false, "You may only gimme50 once a day.. (TestNET coins NOT real Minima)");
				return;
			}
			mLastGimme = timenow;
			
			//construct a special transaction that pays 50 mini to an address this user controls..
			Address addr1 = getMainDB().getUserDB().getCurrentAddress(this);
			Address addr2 = getMainDB().getUserDB().getCurrentAddress(this);
			
			//Now create a transaction that always pays out..
			Transaction trans = new Transaction();
			
			//The Witness..
			Witness wit = new Witness();
					
			//Create the correct inputs..
			Coin in = new Coin(gimme50.COINID_INPUT,Address.TRUE_ADDRESS.getAddressData(),new MiniNumber("50"), Coin.MINIMA_TOKENID);
			
			//Add to the transaction
			trans.addInput(in);
			wit.addScript(Address.TRUE_ADDRESS.getScript(), in.getAddress().getLength()*8);
			
			//Create a small variance in amount.. so coinid is different per coin
			MiniNumber rr = new MiniNumber(new Random().nextLong()).mult(MiniNumber.MINI_UNIT);
			MiniNumber out1 = new MiniNumber("25").sub(rr);
			MiniNumber out2 = new MiniNumber("25").add(rr);
			
			//And send to the new addresses
			trans.addOutput(new Coin(Coin.COINID_OUTPUT,addr1.getAddressData(),out1, Coin.MINIMA_TOKENID));
			trans.addOutput(new Coin(Coin.COINID_OUTPUT,addr2.getAddressData(),out2, Coin.MINIMA_TOKENID));
			
			//Notify listeners that Mining is starting...
			PostDAPPStartMining(trans);
			
			//Now send it..
			Message mine = new Message(ConsensusHandler.CONSENSUS_SENDTRANS, zMessage)
								.addObject("transaction", trans)
								.addObject("witness", wit);
			InputHandler.addResponseMesage(mine, zMessage);
			
			PostMessage(mine);
			
		}else if(zMessage.isMessageType(CONSENSUS_TOKENCREATE)) {
			//Get the amount
			String amount 		= zMessage.getString("amount");
			String name  	 	= zMessage.getString("name");
			String description  = zMessage.getString("description");
			String icon  	 	= zMessage.getString("icon");
			String proof  	 	= zMessage.getString("proof");
			String script       = zMessage.getString("script");
			
			//The  token create coin id..
			MiniData tok  		= Coin.TOKENID_CREATE;
			MiniData changetok 	= Coin.MINIMA_TOKENID;
			
			//Are we specifying the number of decimal places.. default to 8
			int decimalplaces = 8;
			int decimalpoint = amount.indexOf(".");
			if(decimalpoint != -1) {
				String decs   = amount.substring(decimalpoint+1);
				decimalplaces = Integer.parseInt(decs);
			}
			
			//The actual amount of tokens..
			MiniNumber totaltoks = new MiniNumber(amount).floor(); 
			MiniNumber totaldecs = MiniNumber.TEN.pow(decimalplaces); 
			
			//How much Minima will it take to colour.. 
			MiniNumber colorminima = MiniNumber.MINI_UNIT.mult(totaldecs).mult(totaltoks);
			
			//What is the scale..
			int scale = MiniNumber.MAX_DECIMAL_PLACES - decimalplaces;
			
			//The actual amount of Minima that needs to be sent
			MiniNumber sendamount = new MiniNumber(colorminima);
			
			//How much do we have..
			ArrayList<Coin> confirmed = getMainDB().getTotalSimpleSpendableCoins(Coin.MINIMA_TOKENID);
			
			//Select the coins to use in the transaction
			ArrayList<Coin> selectedCoins = selectCoins(confirmed, sendamount);
		
			//Do we have enough funds..
			if(selectedCoins.size()==0) {
				//Sum the confirmed coins..
				MiniNumber conftotal = new MiniNumber();
				for(Coin cc : confirmed) {
					conftotal = conftotal.add(cc.getAmount());
				}
				
				//Insufficient funds!
				InputHandler.endResponse(zMessage, false, "Insufficient funds! You only have : "+conftotal);
				return;
			}
			
			//How much are we sending
			MiniNumber total = new MiniNumber();
			for(Coin cc : selectedCoins) {
				total = total.add(cc.getAmount());
			}
			
			//Get a new address to receive the tokens..
			Address recipient = getMainDB().getUserDB().getCurrentAddress(this);
			
			//Blank address - check change is non-null
			Address change = new Address(); 
			if(!total.isEqual(sendamount)) {
				change = getMainDB().getUserDB().getCurrentAddress(this);
			}
			
			//CHECK NAME of TOKEN IS VALID!
			//TODO Important! - Check token name and details are valid.. add function to TokenProof
			
			//Create the JSON descriptor..
			JSONObject tokenjson = new JSONObject();
			tokenjson.put("name", name);
			if(!description.equals("")) {
				tokenjson.put("description", description);	
			}
			if(!icon.equals("")) {
				tokenjson.put("icon", icon);	
			}
			if(!proof.equals("")) {
				tokenjson.put("proof", proof);	
			}
			
			//Create the token gen details
			TokenProof tokengen = new TokenProof(Coin.COINID_OUTPUT, 
												 new MiniNumber(scale+""), 
												 sendamount, 
												 new MiniString(tokenjson.toString()),
												 new MiniString(script));
			
			//Create the Transaction
			Message ret = getMainDB().createTransaction(sendamount, recipient, change, selectedCoins, tok, changetok,tokengen);
			
			//get the Transaction
			Transaction trans = (Transaction) ret.getObject("transaction");
			
			//Final check..
			if(getMainDB().checkTransactionForMining(trans)) {
				InputHandler.endResponse(zMessage, false, "ERROR double spend coin in mining pool.");
				return;
			}
			
			//Add all the inputs to the mining..
			getMainDB().addMiningTransaction(trans);
			
			//Notify listeners that Mining is starting...
			PostDAPPStartMining(trans);
			
			//Continue the log output trail
			InputHandler.addResponseMesage(ret, zMessage);
			
			//Send it..
			PostMessage(ret);
		}
	}	
	
	/**
	 * Coin Selection Algorithm..
	 * 
	 * Which coins to use when sending a transaction
	 * Expects all the coins to be of the same tokenid
	 */
	public static ArrayList<Coin> selectCoins(ArrayList<Coin> zAllCoins, MiniNumber zAmountRequired){
		ArrayList<Coin> ret = new ArrayList<>();
		
		//First sort the coins by size..
		Collections.sort(zAllCoins, new Comparator<Coin>() {
			@Override
			public int compare(Coin zCoin1, Coin zCoin2) {
				MiniNumber amt1 = zCoin1.getAmount();
				MiniNumber amt2 = zCoin2.getAmount();
				return amt2.compareTo(amt1);
			}
		});

		//Now go through and pick a coin big enough.. but keep looking for smaller coins  
		boolean found    = false;
		Coin currentcoin = null;
		for(Coin cc : zAllCoins) {
			if(cc.getAmount().isMoreEqual(zAmountRequired)) {
				found = true;
				currentcoin = cc;
			}else {
				//Not big enough - all others will be smaller..
				break;
			}
		}
		
		//Did we find one..
		MiniNumber tot = MiniNumber.ZERO;
		if(found) {
			ret.add(currentcoin);
			tot = currentcoin.getAmount();
		}else {
			//Will need to add up multiple coins..
			for(Coin cc : zAllCoins) {
				ret.add(cc);
				tot = tot.add(cc.getAmount());
				
				if(tot.isMoreEqual(zAmountRequired)) {
					break;
				}
			}
		}
		
		//Did we reach the required amount..
		if(tot.isMoreEqual(zAmountRequired)) {
			return ret;
		}
		
		//Not enough funds
		return new ArrayList<Coin>();
	}
	
	
	
	/**
	 * Post a message to all the MiniDAPPs
	 * @param zJSON
	 */
	public void PostDAPPJSONMessage(JSONObject zJSON) {
		if(Main.getMainHandler().getNetworkHandler().getDAPPManager() == null) {
			//ERROR - calling too soon..
			MinimaLogger.log("ERROR - NULL DAPPManager to send message to.. "+zJSON);
			return;
		}
		
		//Notify DAPPS of this message
		Message wsmsg = new Message(DAPPManager.DAPP_MINIDAPP_POSTALL).addObject("message", zJSON);
		Main.getMainHandler().getNetworkHandler().getDAPPManager().PostMessage(wsmsg);
	}
	
	public void PostDAPPStartMining(Transaction zTrans) {
		//Notify listeners that Mining is starting...
		JSONObject mining = new JSONObject();
		mining.put("event","txpowstart");
		mining.put("transaction",zTrans.toJSON());
		PostDAPPJSONMessage(mining);
	}
	
	public void PostDAPPEndMining(Transaction zTrans) {
		//Notify listeners that Mining is starting...
		JSONObject mining = new JSONObject();
		mining.put("event","txpowend");
		mining.put("transaction",zTrans.toJSON());
		PostDAPPJSONMessage(mining);
	}
	
	public static void main(String[] zArgs) {
		Coin cc1 = new Coin(new MiniData(), new MiniData(), new MiniNumber(1), new MiniData());
		Coin cc2 = new Coin(new MiniData(), new MiniData(), new MiniNumber(2), new MiniData());
		Coin cc3 = new Coin(new MiniData(), new MiniData(), new MiniNumber(3), new MiniData());
		
		ArrayList<Coin> coins = new ArrayList<>();
		coins.add(cc1);
		coins.add(cc2);
		coins.add(cc3);
		
		System.out.println(coins.toString());
		
		ArrayList<Coin> ret = selectCoins(coins, new MiniNumber("3.5"));
		System.out.println(ret.toString());
		
		
	}
	
	
}
