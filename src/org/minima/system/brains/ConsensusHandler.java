package org.minima.system.brains;

import java.math.BigDecimal;
import java.util.ArrayList;

import org.minima.NativeListener;
import org.minima.database.MinimaDB;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.TokenDetails;
import org.minima.objects.Transaction;
import org.minima.objects.TxPOW;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniHash;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.SystemHandler;
import org.minima.system.external.ProcessManager;
import org.minima.system.input.InputHandler;
import org.minima.system.input.functions.gimme50;
import org.minima.system.network.NetClient;
import org.minima.system.network.NetClientReader;
import org.minima.system.network.NetworkHandler;
import org.minima.system.tx.TXMiner;
import org.minima.utils.Crypto;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class ConsensusHandler extends SystemHandler {

	/**
	 * Main processing loop for a txpow message
	 */
	public static final String CONSENSUS_PROCESSTXPOW 		   = "CONSENSUS_PROCESSTXPOW";
	public static final String CONSENSUS_PRE_PROCESSTXPOW 	   = "CONSENSUS_PREPROCESSTXPOW";
	
	public static final String CONSENSUS_SENDTRANS 			   = "CONSENSUS_SENDTRANS";
	public static final String CONSENSUS_CREATETRANS 		   = "CONSENSUS_CREATETRANS";
	
	/**
	 * Create Tokens
	 */
	public static final String CONSENSUS_CREATETOKEN 		= "CONSENSUS_CREATETOKEN";
	
	/**
	 * Other functions
	 */
	public static final String CONSENSUS_PRINTCHAIN 		= "CONSENSUS_PRINTCHAIN";
	public static final String CONSENSUS_STATUS 			= "CONSENSUS_STATUS";
	
	
	/**
	 * Notification Messages
	 */
	public static final String CONSENSUS_NOTIFY_QUIT 	    = "CONSENSUS_NOTIFY_QUIT";
	public static final String CONSENSUS_NOTIFY_BALANCE 	= "CONSENSUS_NOTIFY_BALANCE";
	public static final String CONSENSUS_NOTIFY_NEWBLOCK 	= "CONSENSUS_NOTIFY_NEWBLOCK";
	public static final String CONSENSUS_NOTIFY_RELCOIN 	= "CONSENSUS_NOTIFY_RELCOIN";
	
	//DEBUG FUNCTION
	public static final String CONSENSUS_GIMME50 			= "CONSENSUS_GIMME50";
	public static final String CONSENSUS_STRESS_TRANS 		= "CONSENSUS_STRESS_TRANS";
	
		
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
	 * Main Constructor
	 * @param zMain
	 */
	public ConsensusHandler(Main zMain) {
		super(zMain, "CONSENSUS");
		
//		mLogON = true;
		
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
	
//	public void updateListeners(String zMessageType) {
//		Message msg = new Message(zMessageType);
//		for(NativeListener listen : mListeners) {
//			listen.processMessage(msg);
//		}
//	}
	
	/**
	 * Are we initialising the Genesis block
	 */
	public void genesis() {
		getMainDB().DoGenesis();
	}
	
	private MinimaDB getMainDB() {
		return mMainDB;
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		/**
		 * Main processing function.. can be called recursively..
		 */
		if ( zMessage.isMessageType(CONSENSUS_PROCESSTXPOW) ) {
			//A TXPOW - that has been checked already and added to the DB
			TxPOW txpow = (TxPOW) zMessage.getObject("txpow");
			
			//Process
			getMainDB().processTxPOW(txpow);
		
			//Print the tree..
			if(mPrintChain) {
				Message print = new Message(ConsensusPrint.CONSENSUS_PRINTCHAIN_TREE);
				InputHandler.addResponseMesage(print, zMessage);
				PostMessage(print);
			}
			
			/**
			 * One time run the first time you see a txpow..
			 */
		}else if ( zMessage.isMessageType(CONSENSUS_PRE_PROCESSTXPOW) ) {
			//The TXPOW
			TxPOW txpow = (TxPOW) zMessage.getObject("txpow");
			
			//Could be 	an internal PULSE message
			if(txpow.getTransaction().isEmpty() && !txpow.isBlock()) {
				//It's Pulse.. send it.. 
				//..
				return;
			}
			
			//It's something.. Make sure has been added - could be an internal message - won't add again if already there.
			getMainDB().addNewTxPow(txpow);
			
			//Back it up!
			getMainHandler().getBackupManager().backupTxpow(txpow);
			
			//Only do this once..
			boolean relevant = false;
			if(txpow.isTransaction()) {
				relevant = checkTransactionRelevant(txpow, zMessage);
			}
			
			//If it's relevant then do a backup..
			if(relevant) {
				Message backup = new Message(ConsensusBackup.CONSENSUSBACKUP_BACKUP);
				InputHandler.addResponseMesage(backup, zMessage);
				
				//Do a backup..
				getMainHandler().getConsensusHandler().PostMessage(backup);
			
				//Notify those listening..
				getMainHandler().getNetworkHandler().PostMessage(NetworkHandler.NETWORK_NOTIFY);
			}
			
			//Message for the clients
			Message msg  = new Message(NetClient.NETCLIENT_SENDOBJECT).addObject("type", NetClientReader.NETMESSAGE_TXPOWID).addObject("object", txpow.getTxPowID());
			InputHandler.addResponseMesage(msg, zMessage);
			
			Message netw = new Message(NetworkHandler.NETWORK_SENDALL).addObject("message", msg);
			InputHandler.addResponseMesage(netw, zMessage);
			
			//Post It..
			getMainHandler().getNetworkHandler().PostMessage(netw);
			
			//Process it
			Message proc = new Message(ConsensusHandler.CONSENSUS_PROCESSTXPOW).addObject("txpow", txpow);
			InputHandler.addResponseMesage(proc, zMessage);
			PostMessage(proc);

			//Tell the listeners.. ?
			if(txpow.isBlock()) {
				Message upd = new Message(CONSENSUS_NOTIFY_NEWBLOCK).addObject("txpow", txpow);
				InputHandler.addResponseMesage(upd, zMessage);
				updateListeners(upd);
			}
		
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
			 * Transaction management
			 */
		}else if ( zMessage.isMessageType(CONSENSUS_SENDTRANS) ) {
			//get The Transaction
			Transaction trans = (Transaction) zMessage.getObject("transaction");
		
			//Get the Witness data if a valid transaction and not just an off chain zero transaction
			Witness wit = (Witness) zMessage.getObject("witness");
			
			//Add it to the current TX-POW
			TxPOW txpow = getMainDB().getCurrentTxPow(trans, wit);
			
			//Send it to the Miner..
			Message mine = new Message(TXMiner.TXMINER_MINETXPOW).addObject("txpow", txpow);
			InputHandler.addResponseMesage(mine, zMessage);
			
			getMainHandler().getMiner().PostMessage(mine);
		
			JSONObject resp = InputHandler.getResponseJSON(zMessage);
			resp.put("txpow", txpow);
			
			InputHandler.endResponse(zMessage, true, "");
			
		}else if ( zMessage.isMessageType(CONSENSUS_CREATETRANS) ) {
			//How much to who ?
			String address 		= zMessage.getString("address");
			String amount  		= zMessage.getString("amount");
			String tokenid 	   	= zMessage.getString("tokenid");
			
			//The Token Hash
			MiniHash tok       		= new MiniHash(tokenid);
			MiniHash changetok 		= new MiniHash(tokenid);
			
			//Is this a token amount or a minima amount
			TokenDetails tokendets = null;
			if(!tok.isExactlyEqual(Coin.MINIMA_TOKENID)) {
				//It's a token.. scale it..
				MiniNumber samount = new MiniNumber(amount);
				
				//Now divide by the scale factor..
				tokendets = getMainDB().getUserDB().getTokenDetail(new MiniHash(tokenid));
				
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
			if(tok.isExactlyEqual(Coin.TOKENID_CREATE)) {
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
				
			}else {
				//Continue constructing the transaction - outputs don't need scripts
				Address recipient= new Address(new MiniHash(address));
				
				//Blank address - check change is non-null
				Address change = new Address(); 
				if(!total.isEqual(sendamount)) {
					change = getMainDB().getUserDB().newSimpleAddress();
				}
				
				//Create the Transaction
				Message ret = getMainDB().createTransaction(sendamount, recipient, change, confirmed, tok, changetok);
				
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
			
		/**
		 * Other Functions
		 */
		}else if(zMessage.isMessageType(CONSENSUS_STRESS_TRANS)) {
			//Send a random transaction!
			Transaction trans = new Transaction();
			Witness wit = new Witness();
			
			Coin in = new Coin(gimme50.COINID_INPUT,Address.TRUE_ADDRESS.getAddressData(),new MiniNumber("1"), MiniHash.ZERO32);
			trans.addInput(in);
			wit.addScript(Address.TRUE_ADDRESS.getScript());
			
			//And send to the new address
			Address outaddr = new Address(new MiniHash(MiniData.getRandomData(32).getData()));
			Coin out = new Coin(Coin.COINID_OUTPUT,outaddr.getAddressData(),new MiniNumber("1"), MiniHash.ZERO32);
			trans.addOutput(out);
			
			//Now send it..
			Message mine = new Message(ConsensusHandler.CONSENSUS_SENDTRANS)
								.addObject("transaction", trans)
								.addObject("witness", wit);
			InputHandler.addResponseMesage(mine, zMessage);
			
			PostMessage(mine);
		
		}else if(zMessage.isMessageType(CONSENSUS_GIMME50)) {
			//construct a special transaction that pays 50 mini to an address this user controls..
			Address addr = getMainDB().getUserDB().newSimpleAddress();
			
			//Now create a transaction that always pays out..
			Transaction trans = new Transaction();
			
			//The Witness..
			Witness wit = new Witness();
					
			//Create the correct inputs..
			Coin in = new Coin(gimme50.COINID_INPUT,Address.TRUE_ADDRESS.getAddressData(),new MiniNumber("50"), MiniHash.ZERO32);
			
			//Add to the transaction
			trans.addInput(in);
			
			//And the Witness.. no parameters required..
			wit.addScript(Address.TRUE_ADDRESS.getScript());
			
			//And send to the new address
			Coin out = new Coin(Coin.COINID_OUTPUT,addr.getAddressData(),new MiniNumber("50"), MiniHash.ZERO32);
			trans .addOutput(out);
			
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
			MiniHash tok  		= Coin.TOKENID_CREATE;
			MiniHash changetok 	= Coin.MINIMA_TOKENID;
			
			//Get a new address to receive the tokens..
			Address recipient = getMainDB().getUserDB().newSimpleAddress();
			
			//How much Minima will it take to colour.. for now lets stay under 0.001 minima
			//This is not protocol specific and can change later
			BigDecimal max    = new BigDecimal("0.001");
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
				
				//Create the Transaction
				Message ret = getMainDB().createTransaction(sendamount, recipient, change, confirmed, tok, changetok);
				
				//Get the witness and add relevant info..
				Witness wit = (Witness) ret.getObject("witness");
				
				//Create the token gen details
				TokenDetails tgen = new TokenDetails(Coin.COINID_OUTPUT, 
													 new MiniNumber(scale+""), 
													 sendamount, 
													 new MiniString(name));
				
				//Set it
				wit.setTokenGenDetails(tgen);
				
				//Continue the log output trail
				InputHandler.addResponseMesage(ret, zMessage);
				
				//Send it..
				PostMessage(ret);
			}
			
		}
	}
	
	/**
	 * Is this a relevant transaction for us..
	 * 
	 * @param zTrans
	 * @return
	 */
	public boolean checkTransactionRelevant(TxPOW zTxPOW,Message zOriginal) {
		Transaction trans = zTxPOW.getTransaction();
		
		ArrayList<Coin> ins  = trans.getAllInputs();
		ArrayList<Coin> outs = trans.getAllOutputs();
		
		//The HASH of the Transaction.. needed for coinid
		MiniHash transhash = Crypto.getInstance().hashObject(trans);
		
		//Check them - adding the script to outputs we own
		boolean rel = false;
		MiniNumber tot = MiniNumber.ZERO;
		for(Coin in : ins) {
			if(getMainDB().getUserDB().isAddressRelevant(in.getAddress())) {
//				SimpleLogger.log("MINIMA SPENT : "+in.getAmount()+" ( unconfirmed )");
				rel = true;
				
				Message relmsg = new Message(ProcessManager.PROCESS_RELCOIN)
									.addObject("coin", in)
									.addObject("txpowid", zTxPOW.getTxPowID())
									.addObject("transid", transhash)
									.addObject("spent", true);
				InputHandler.addResponseMesage(relmsg, zOriginal);
				
				//And do we need to call a local function..
				getMainHandler().getProcessManager().PostMessage(relmsg);
				
				//Subtract
				tot = tot.sub(in.getAmount());
			}
		}
			
		int len = outs.size();
		for(int i=0;i<len;i++) {
			//get the coin
			Coin out = outs.get(i);
			
			if(getMainDB().getUserDB().isAddressRelevant(out.getAddress())) {
//				SimpleLogger.log("MINIMA RECEIVED : "+out.getAmount()+" ( unconfirmed )");
				rel = true;
				
				//Now calculate the CoinID / TokenID
				MiniHash coinid = Crypto.getInstance().hashObjects(transhash, new MiniByte(i));
				
				//Create a new Coin..
				Coin fullcoin = new Coin(coinid, out.getAddress(), out.getAmount(), out.getTokenID());
				
				Message relmsg = new Message(ProcessManager.PROCESS_RELCOIN)
									.addObject("coin", fullcoin)
									.addObject("txpowid", zTxPOW.getTxPowID())
									.addObject("transid", transhash)
									.addObject("spent", false);
				InputHandler.addResponseMesage(relmsg, zOriginal);
				
				//And do we need to call a local function..
				getMainHandler().getProcessManager().PostMessage(relmsg);
				
				//Add
				tot = tot.add(out.getAmount());
			}
		}
		
		//Is it relevant..
		if(rel) {
			//Update the Native Listeners..
			Message upd = new Message(CONSENSUS_NOTIFY_BALANCE).addString("change", tot.toString());
			InputHandler.addResponseMesage(upd, zOriginal);
			updateListeners(upd);
			
			//And do we need to call a local function..
			Message command = new Message(ProcessManager.PROCESS_TXNCALL)
									.addObject("transaction", trans)
									.addObject("transid", transhash)
									.addObject("txpowid", zTxPOW.getTxPowID())
									.addObject("total", tot);
			InputHandler.addResponseMesage(command, zOriginal);
			
			getMainHandler().getProcessManager().PostMessage(command);
		}
		
		return rel;
	}
}
