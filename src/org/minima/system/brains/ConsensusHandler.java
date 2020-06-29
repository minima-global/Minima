package org.minima.system.brains;

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Hashtable;

import org.minima.GlobalParams;
import org.minima.database.MinimaDB;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.Transaction;
import org.minima.objects.TxPoW;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.objects.proofs.TokenProof;
import org.minima.system.Main;
import org.minima.system.NativeListener;
import org.minima.system.SystemHandler;
import org.minima.system.input.InputHandler;
import org.minima.system.input.functions.gimme50;
import org.minima.system.network.NetClient;
import org.minima.system.network.NetClientReader;
import org.minima.system.network.NetworkHandler;
import org.minima.system.txpow.TxPoWChecker;
import org.minima.system.txpow.TxPoWMiner;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.TimerMessage;

public class ConsensusHandler extends SystemHandler {

	/**
	 * Main processing loop for a txpow message
	 */
	public static final String CONSENSUS_PROCESSTXPOW 		   = "CONSENSUS_PROCESSTXPOW";
	public static final String CONSENSUS_PRE_PROCESSTXPOW 	   = "CONSENSUS_PREPROCESSTXPOW";
	
	/**
	 * Auto backup every 10 minutes..
	 */
	public static final String CONSENSUS_AUTOBACKUP 	       = "CONSENSUS_AUTOBACKUP";
	
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
	 * Create Tokens
	 */
	public static final String CONSENSUS_CREATETOKEN 		= "CONSENSUS_CREATETOKEN";
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
	ArrayList<NativeListener> mListeners;
	
	/**
	 * FLUSH counter 
	 */
	int mFlushCounter = 0;
	
	/**
	 * The Last Gimme50..
	 */
	long mLastGimme = 0;
	public static final long MIN_GIMME50_TIME_GAP = 1000 * 60 * 10;
	
	/**	
	 * Main Constructor
	 * @param zMain
	 */
	public ConsensusHandler(Main zMain) {
		super(zMain, "CONSENSUS");
		
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
		PostTimerMessage(new TimerMessage(2000, CONSENSUS_MINEBLOCK));
	
		//Redo every 10 minutes..
		PostTimerMessage(new TimerMessage(10 * 60 * 1000, CONSENSUS_AUTOBACKUP));
	}
	
	public void setBackUpManager() {
		getMainDB().setBackupManager(getMainHandler().getBackupManager());
	}
	
	/**
	 * Listener Functions
	 */
	public void clearListeners() {
		mListeners.clear();
	}
	
	public void addListener(NativeListener zListen) {
		mListeners.add(zListen);
	}
	
	public void removeListener(NativeListener zListen) {
		mListeners.remove(zListen);
	}
	
	public void updateListeners(Message zMessage) {
		for(NativeListener listen : mListeners) {
			listen.processMessage(zMessage);
		}
	}
	
	/**
	 * Are we initialising the Genesis block
	 */
	public void genesis() {
		getMainDB().DoGenesis();
	}
	
	/**
	 * Hard code whether we can reset the chain on intro messages
	 * 
	 * @param zHardResetAllowed
	 */
	public void setHardResetAllowed(boolean zHardResetAllowed){
		mConsensusNet.setHardResest(zHardResetAllowed);
	}
	
	private MinimaDB getMainDB() {
		return mMainDB;
	}
	
	public boolean isInitialSyncComplete() {
		return mConsensusNet.mInitialSync;
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		/**
		 * Main processing function.. 
		 */
		if ( zMessage.isMessageType(CONSENSUS_PROCESSTXPOW) ) {
			//A TXPOW - that has been checked already and added to the DB
			TxPoW txpow = (TxPoW) zMessage.getObject("txpow");
			
			//What's the current chain tip..
			MiniData oldtip = getMainDB().getMainTree().getChainTip().getTxPowID();
			
			//Process
			getMainDB().processTxPOW(txpow);
		
			//What's the new chain tip..
			TxPoW newtip = getMainDB().getMainTree().getChainTip().getTxPow();
			
			//Has there been a change
			if(!oldtip.isEqual(newtip.getTxPowID())) {
				//Notify..
				updateListeners(new Message(CONSENSUS_NOTIFY_NEWBLOCK).addObject("txpow", newtip));
			
				//Update the web listeners..
				Message statusupdate = new Message(ConsensusPrint.CONSENSUS_STATUS).addBoolean("hard", true);
				PostMessage(statusupdate);
				
				//Do the balance.. Update listeners if changed..
				Message balanceupdate = new Message(ConsensusPrint.CONSENSUS_BALANCE).addBoolean("hard", true);
				PostMessage(balanceupdate);
			}
			
			//MemPool Flush Counter... 
			if(txpow.isBlock()) {
				//Every 5 minutes or so check if you have all the parents and txns in blocks..
				if(mFlushCounter++ > 16) {
					mFlushCounter = 0;
					
					//Post a flush message.. could be stuck missing a block..
					PostMessage(new Message(ConsensusUser.CONSENSUS_FLUSHMEMPOOL));
				}
			}
			
			//Print the tree..
			if(mPrintChain) {
				Message print = new Message(ConsensusPrint.CONSENSUS_PRINTCHAIN_TREE).addBoolean("systemout", true);
				PostMessage(print);
			}
						
			/**
			 * One time run the first time you see a txpow..
			 */
		}else if ( zMessage.isMessageType(CONSENSUS_PRE_PROCESSTXPOW) ) {
			//The TXPOW
			TxPoW txpow = (TxPoW) zMessage.getObject("txpow");
			
			//Back it up!
			getMainHandler().getBackupManager().backupTxpow(txpow);
			
			//Notify the WebSocket Listeners
			JSONObject newtrans = new JSONObject();
			newtrans.put("event","newtransaction");
			newtrans.put("txpow",txpow.toJSON());
			
			Message msg = new Message(NetworkHandler.NETWORK_WS_NOTIFY);
			msg.addString("message", newtrans.toString());
			getMainHandler().getNetworkHandler().PostMessage(msg);
			
			//Process it
			PostMessage(new Message(ConsensusHandler.CONSENSUS_PROCESSTXPOW).addObject("txpow", txpow));
			
			//Only do this once..
			boolean relevant = false;
			if(txpow.isTransaction()) {
				relevant = getMainDB().getUserDB().isTransactionRelevant(txpow.getTransaction());
			}
			
			//If it's relevant then do a backup..
			if(relevant) {
				//Get the Token Amounts..
				Hashtable<String, MiniNumber> tokamt = getMainDB().getTransactionTokenAmounts(txpow);
				
				//Store ion the database..
				getMainDB().getUserDB().addToHistory(txpow,tokamt);
				
				//Notify those listening..
				updateListeners(new Message(CONSENSUS_NOTIFY_BALANCE));
				
				//Do the balance.. Update listeners if changed..
				Message balanceupdate = new Message(ConsensusPrint.CONSENSUS_BALANCE).addBoolean("hard", true);
				PostMessage(balanceupdate);
			}
			
			//Message for ALL the clients
			Message netmsg  = new Message(NetClient.NETCLIENT_SENDTXPOWID).addObject("txpowid", txpow.getTxPowID());
			Message netw    = new Message(NetworkHandler.NETWORK_SENDALL).addObject("message", netmsg);
			
			//Post It..
			getMainHandler().getNetworkHandler().PostMessage(netw);
			
		}else if ( zMessage.isMessageType(CONSENSUS_AUTOBACKUP) ) {
			//Backup the system..
			PostMessage(ConsensusBackup.CONSENSUSBACKUP_BACKUP);
			
			//Redo every 10 minutes..
			PostTimerMessage(new TimerMessage(10 * 60 * 1000, CONSENSUS_AUTOBACKUP));
			
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
			getMainHandler().getMiner().setAutoMining(mining);
			
			JSONObject resp = InputHandler.getResponseJSON(zMessage);
			resp.put("automining", mining);			
			InputHandler.endResponse(zMessage, true, "");
		
		}else if ( zMessage.isMessageType(CONSENSUS_MINEBLOCK) ) {
			//DEBUG MODE - only mine a block when you make a transction..
			if(GlobalParams.MINIMA_ZERO_DIFF_BLK) {return;}
				
			//Are we Mining..
			if(!getMainHandler().getMiner().isAutoMining()) {
				PostTimerMessage(new TimerMessage(10000, CONSENSUS_MINEBLOCK));
				return;
			}
			
			//Fresh TXPOW
			TxPoW txpow = getMainDB().getCurrentTxPow(new Transaction(), new Witness(), new JSONArray());
			
			//Send it to the Miner..
			Message mine = new Message(TxPoWMiner.TXMINER_MEGAMINER).addObject("txpow", txpow);
			
			//Post to the Miner
			getMainHandler().getMiner().PostMessage(mine);
		
		}else if ( zMessage.isMessageType(CONSENSUS_DEBUGMINE) ) {
			//Mine one single block.. 
			TxPoW txpow = getMainDB().getCurrentTxPow(new Transaction(), new Witness(), new JSONArray());
			
			//Send it to the Miner..
			Message mine = new Message(TxPoWMiner.TXMINER_DEBUGBLOCK).addObject("txpow", txpow);
			
			//Continue the log output trail
			InputHandler.addResponseMesage(mine, zMessage);
			
			//Post to the Miner
			getMainHandler().getMiner().PostMessage(mine);
			
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
				//Reject
				InputHandler.endResponse(zMessage, false, "Invalid Signatures! - TXNAUTO must be done AFTER adding state variables ?");
				return;
			}
			
			//Final check of the mempool coins..
			if(getMainDB().checkTransactionForMempoolCoins(trans)) {
				//No GOOD!
				InputHandler.endResponse(zMessage, false, "ERROR double spend coin in mempool.");
				return;
			}
			
			
			//CHECK THE SIZE
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			DataOutputStream dos = new DataOutputStream(baos);
			txpow.writeDataStream(dos);
			dos.flush();
			int txpowsize = baos.toByteArray().length;
			dos.close();
			baos.close();
			
			//Add the size..
			resp.put("size", txpowsize);
			resp.put("inputs", txpow.getTransaction().getAllInputs().size());
			resp.put("outputs", txpow.getTransaction().getAllOutputs().size());
			
			if(txpowsize > NetClientReader.MAX_TXPOW) {
				//Add the TxPoW
				resp.put("transaction", txpow.getTransaction());
				
				//ITS TOO BIG!
				InputHandler.endResponse(zMessage, false, "YOUR TXPOW TRANSACTION IS TOO BIG! MAX SIZE : "+NetClientReader.MAX_TXPOW);
				
				return;
			}
					
			//Add to the list of Mined Coins!
			boolean newtrans = getMainDB().addMiningTransaction(txpow.getTransaction());
			if(newtrans) {
				//Notify listeners that Mining is starting...
				JSONObject mining = new JSONObject();
				mining.put("event","txpowstart");
				mining.put("transaction",txpow.getTransaction().toJSON().toString());
				
				Message wsmsg = new Message(NetworkHandler.NETWORK_WS_NOTIFY).addString("message", mining.toString());
				getMainHandler().getNetworkHandler().PostMessage(wsmsg);
			}
			
			//Send it to the Miner.. This is the ONLY place this happens..
			Message mine = new Message(TxPoWMiner.TXMINER_MINETXPOW).addObject("txpow", txpow);
			getMainHandler().getMiner().PostMessage(mine);
		
			//Add the TxPoW
			resp.put("txpow", txpow);
			
			InputHandler.endResponse(zMessage, true, "Send Success");
			
			//OK - Some new outputs addresses for sure.. do a backup..
			PostMessage(ConsensusBackup.CONSENSUSBACKUP_BACKUPUSER);
			
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
				samount = samount.div(tokendets.getScaleFactor());
				
				//And set the new value..
				amount = samount.toString();
			}
			
			//Send details..
			MiniNumber sendamount 	= new MiniNumber(amount);
			
			//How much do we have..
			MiniNumber total = new MiniNumber(); 
			ArrayList<Coin> confirmed = null;
			if(tok.isEqual(Coin.TOKENID_CREATE)) {
				confirmed = getMainDB().getTotalSimpleSpendableCoins(Coin.MINIMA_TOKENID);
				changetok = Coin.MINIMA_TOKENID;
			}else {
				confirmed = getMainDB().getTotalSimpleSpendableCoins(tok);
			}
			
			for(Coin cc : confirmed) {
				total = total.add(cc.getAmount());
			}

			//Do we have that much..
			if(total.isLess(sendamount)) {
				//Insufficient funds!
				if(!tokenid.equals(Coin.MINIMA_TOKENID.to0xString())) {
					total = total.mult(tokendets.getScaleFactor());
					InputHandler.endResponse(zMessage, false, "Insufficient funds! You only have : "+total);
				}else {
					InputHandler.endResponse(zMessage, false, "Insufficient funds! You only have : "+total);
				}
				
				return;
				
			}else{
				//Continue constructing the transaction - outputs don't need scripts
				Address recipient= new Address(new MiniData(address));
				
				//Blank address - check change is non-null
				Address change = new Address(); 
				if(!total.isEqual(sendamount)) {
					change = getMainDB().getUserDB().newSimpleAddress();
				}
				
				//Create the Transaction
				Message ret = getMainDB().createTransaction(sendamount, recipient, change, confirmed, tok, changetok,null);
				
				//Is this a token transaction
				if(tokendets != null) {
					//Get the witness and add relevant info..
					Witness wit = (Witness) ret.getObject("witness");
					
					//Get the token details..
					wit.addTokenDetails(tokendets);
				}
				
				//Get the message ready
				InputHandler.addResponseMesage(ret, zMessage);
				
				//Send it..
				PostMessage(ret);
			}

		}else if(zMessage.isMessageType(CONSENSUS_FINISHED_MINE)) {
			//The TXPOW
			TxPoW txpow = (TxPoW) zMessage.getObject("txpow");
			
			//Remove from the List of Mined transactions..
			getMainDB().remeoveMiningTransaction(txpow.getTransaction());
			
			//And now forward the message to the single entry point..
			Message msg = new Message(ConsensusNet.CONSENSUS_NET_CHECKSIZE_TXPOW).addObject("txpow", txpow);
			PostMessage(msg);
			
			//Notify listeners that Mining is starting...
			JSONObject mining = new JSONObject();
			mining.put("event","txpowend");
			mining.put("transaction",txpow.getTransaction().toJSON().toString());
			
			Message wsmsg = new Message(NetworkHandler.NETWORK_WS_NOTIFY).addString("message", mining.toString());
			getMainHandler().getNetworkHandler().PostMessage(wsmsg);
			
		}else if(zMessage.isMessageType(CONSENSUS_GIMME50)) {
			//Check time
			long timenow = System.currentTimeMillis();
			if(timenow - mLastGimme < MIN_GIMME50_TIME_GAP) {
				//You can only do one of these every 10 minutes..
				InputHandler.endResponse(zMessage, false, "You may only gimme50 once every 10 minutes");
				return;
			}
			mLastGimme = timenow;
			
			//construct a special transaction that pays 50 mini to an address this user controls..
			Address addr1 = getMainDB().getUserDB().newSimpleAddress();
			Address addr2 = getMainDB().getUserDB().newSimpleAddress();
			
			//Now create a transaction that always pays out..
			Transaction trans = new Transaction();
			
			//The Witness..
			Witness wit = new Witness();
					
			//Create the correct inputs..
			Coin in = new Coin(gimme50.COINID_INPUT,Address.TRUE_ADDRESS.getAddressData(),new MiniNumber("50"), Coin.MINIMA_TOKENID);
			
			//Add to the transaction
			trans.addInput(in);
			wit.addScript(Address.TRUE_ADDRESS.getScript(), in.getAddress().getLength()*8);
			
			//And send to the new addresses
			trans.addOutput(new Coin(Coin.COINID_OUTPUT,addr1.getAddressData(),new MiniNumber("25"), Coin.MINIMA_TOKENID));
			trans.addOutput(new Coin(Coin.COINID_OUTPUT,addr2.getAddressData(),new MiniNumber("25"), Coin.MINIMA_TOKENID));
			
			//Now send it..
			Message mine = new Message(ConsensusHandler.CONSENSUS_SENDTRANS)
								.addObject("transaction", trans)
								.addObject("witness", wit);
			InputHandler.addResponseMesage(mine, zMessage);
			
			PostMessage(mine);
		
		}else if(zMessage.isMessageType(CONSENSUS_CREATETOKEN)) {
			//Get the amount
			String amount 		= zMessage.getString("amount");
			String name  	 	= zMessage.getString("name");
			String script       = zMessage.getString("script");
			
			MiniData tok  		= Coin.TOKENID_CREATE;
			MiniData changetok 	= Coin.MINIMA_TOKENID;
			
			//Get a new address to receive the tokens..
			Address recipient = getMainDB().getUserDB().newSimpleAddress();
			
			//How much Minima will it take to colour.. for now lets stay under 0.001 minima
			//This is not protocol specific and can change later
			BigDecimal max    = new BigDecimal("0.01");
			BigDecimal num    = new BigDecimal(amount);
			BigDecimal actnum = new BigDecimal(amount);
			
			//Cylce to the right size..
			int scale = 0;
			while(actnum.compareTo(max)>0) {
				actnum = actnum.divide(BigDecimal.TEN);
				scale++;
			}
			
			//The actual amount of Minima that needs to be sent
			MiniNumber sendamount = new MiniNumber(actnum);
			
			//How much do we have..
			MiniNumber total = new MiniNumber(); 
			ArrayList<Coin> confirmed = getMainDB().getTotalSimpleSpendableCoins(Coin.MINIMA_TOKENID);
			
			//Add all the available outputs to the list
			for(Coin cc : confirmed) {
				total = total.add(cc.getAmount());
			}

			//Do we have that much..
			if(total.isLess(sendamount)) {
				//Insufficient funds!
				InputHandler.endResponse(zMessage, false, "Insufficient funds! You only have : "+total);
				
			}else {
				//Blank address - check change is non-null
				Address change = new Address(); 
				if(!total.isEqual(sendamount)) {
					change = getMainDB().getUserDB().newSimpleAddress();
				}
				
				//CHECK NAME of TOKEN IS VALID!
				//TODO
				
				//Create the token gen details
				TokenProof tokengen = new TokenProof(Coin.COINID_OUTPUT, 
													 new MiniNumber(scale+""), 
													 sendamount, 
													 new MiniString(name),
													 new MiniString(script));
				
				//Create the Transaction
				Message ret = getMainDB().createTransaction(sendamount, recipient, change, confirmed, tok, changetok,tokengen);
				
				//Continue the log output trail
				InputHandler.addResponseMesage(ret, zMessage);
				
				//Send it..
				PostMessage(ret);
			}
			
		}else if(zMessage.isMessageType(CONSENSUS_TOKENCREATE)) {
			//Get the amount
			String amount 		= zMessage.getString("amount");
			String name  	 	= zMessage.getString("name");
			String description  = zMessage.getString("description");
			String icon  	 	= zMessage.getString("icon");
			String proof  	 	= zMessage.getString("proof");
			String script       = zMessage.getString("script");
			
			/* 
			 * ASSERT FLOOR ( @AMOUNT ) EQ @AMOUNT LET checkout = 0 
			 * WHILE ( checkout LT @TOTOUT ) DO 
			 *  IF GETOUTTOK ( checkout ) EQ @TOKENID THEN 
			 *   LET outamt = GETOUTAMT ( checkout ) 
			 *   ASSERT FLOOR ( outamt ) EQ outamt 
			 *  ENDIF 
			 *  LET checkout = INC ( checkout ) 
			 * ENDWHILE 
			 * RETURN TRUE
			 * 
			 */
			
			MiniData tok  		= Coin.TOKENID_CREATE;
			MiniData changetok 	= Coin.MINIMA_TOKENID;
			
			//Get a new address to receive the tokens..
			Address recipient = getMainDB().getUserDB().newSimpleAddress();
			
			//How much Minima will it take to colour.. for now lets stay under 0.001 minima
			//This is not protocol specific and can change later
			BigDecimal max    = new BigDecimal("0.01");
			BigDecimal num    = new BigDecimal(amount);
			BigDecimal actnum = new BigDecimal(amount);
			
			//Cylce to the right size..
			int scale = 0;
			while(actnum.compareTo(max)>0) {
				actnum = actnum.divide(BigDecimal.TEN);
				scale++;
			}
			
			//The actual amount of Minima that needs to be sent
			MiniNumber sendamount = new MiniNumber(actnum);
			
			//How much do we have..
			MiniNumber total = new MiniNumber(); 
			ArrayList<Coin> confirmed = getMainDB().getTotalSimpleSpendableCoins(Coin.MINIMA_TOKENID);
			
			//Add all the available outputs to the list
			for(Coin cc : confirmed) {
				total = total.add(cc.getAmount());
			}

			//Do we have that much..
			if(total.isLess(sendamount)) {
				//Insufficient funds!
				InputHandler.endResponse(zMessage, false, "Insufficient funds! You only have : "+total);
				
			}else {
				//Blank address - check change is non-null
				Address change = new Address(); 
				if(!total.isEqual(sendamount)) {
					change = getMainDB().getUserDB().newSimpleAddress();
				}
				
				//CHECK NAME of TOKEN IS VALID!
				//TODO
				
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
				Message ret = getMainDB().createTransaction(sendamount, recipient, change, confirmed, tok, changetok,tokengen);
				
				//Continue the log output trail
				InputHandler.addResponseMesage(ret, zMessage);
				
				//Send it..
				PostMessage(ret);
			}
		}
		
	}	
}
