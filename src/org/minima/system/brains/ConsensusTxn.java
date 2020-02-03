package org.minima.system.brains;

import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.coindb.CoinDBRow;
import org.minima.database.userdb.UserDBRow;
import org.minima.miniscript.Contract;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.PubPrivKey;
import org.minima.objects.StateVariable;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniHash;
import org.minima.objects.base.MiniNumber;
import org.minima.system.input.InputHandler;
import org.minima.system.network.NetworkHandler;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class ConsensusTxn {

	/**
	 * Used for the custom Transactions
	 */
	public static final String CONSENSUS_PREFIX 			= "CONSENSUSTXN_";
	
	public static final String CONSENSUS_TXNCREATE 			= CONSENSUS_PREFIX+"TXNCREATE";
	public static final String CONSENSUS_TXNDELETE 			= CONSENSUS_PREFIX+"TXNDELETE";
	public static final String CONSENSUS_TXNLIST 			= CONSENSUS_PREFIX+"TXNLIST";
	
	public static final String CONSENSUS_TXNINPUT 			= CONSENSUS_PREFIX+"TXNINPUT";
	public static final String CONSENSUS_TXNOUTPUT 			= CONSENSUS_PREFIX+"TXNOUTPUT";
	
	public static final String CONSENSUS_TXNSTATEVAR 		= CONSENSUS_PREFIX+"TXNSTATEVAR";
	
	public static final String CONSENSUS_TXNSIGN 			= CONSENSUS_PREFIX+"TXNSIGN";
	public static final String CONSENSUS_TXNVALIDATE 		= CONSENSUS_PREFIX+"TXNVALIDATE";
	
	public static final String CONSENSUS_TXNPOST 			= CONSENSUS_PREFIX+"TXNPOST";
	
	MinimaDB mDB;
	
	ConsensusHandler mHandler;
	
	public ConsensusTxn(MinimaDB zDB, ConsensusHandler zHandler) {
		mDB = zDB;
		mHandler = zHandler;
	}
	
	private MinimaDB getMainDB() {
		return mDB;
	}
	
	private boolean checkTransactionValid(int zTrans) {
		//Get the user row
		UserDBRow row = getMainDB().getUserDB().getUserRow(zTrans);
		
		if(row == null) {
			return false;
		}
		
		return true;
	}
	
	private void listTransactions(Message zMessage) {
		Message list = new Message(CONSENSUS_TXNLIST);
		InputHandler.addResponseMesage(list, zMessage);
		mHandler.PostMessage(list);
	}
	
	public void processMessage(Message zMessage) throws Exception {
		
		/**
		 * Custom Transactions
		 */
		if(zMessage.isMessageType(CONSENSUS_TXNCREATE)) {
			getMainDB().getUserDB().addUserRow();
			
			listTransactions(zMessage);
		
		}else if(zMessage.isMessageType(CONSENSUS_TXNDELETE)) {
			//Which transaction
			int trans    = zMessage.getInteger("transaction");
			
			getMainDB().getUserDB().deleteUserRow(trans);
			
			listTransactions(zMessage);
			
		}else if(zMessage.isMessageType(CONSENSUS_TXNLIST)) {
			JSONArray arr = new JSONArray();
			
			//get all the transactions..
			ArrayList<UserDBRow> rows = getMainDB().getUserDB().getAllRows();
			for(UserDBRow row : rows) {
				arr.add(row.toJSON());
			}
			
			InputHandler.getResponseJSON(zMessage).put("transactions", arr);
			InputHandler.endResponse(zMessage, true, "");
			
		}else if(zMessage.isMessageType(CONSENSUS_TXNINPUT)) {
			//Add input to a custom transaction
			int trans 			= zMessage.getInteger("transaction");
			MiniHash coinid 	= (MiniHash) zMessage.getObject("coinid");
			
			//Check valid..
			if(!checkTransactionValid(trans)) {
				InputHandler.endResponse(zMessage, false, "Invalid TXN chosen : "+trans);
				return;
			}
			
			//Get the Transaction..
			Transaction trx =  getMainDB().getUserDB().getUserRow(trans).getTransaction();
			Witness wit     =  getMainDB().getUserDB().getUserRow(trans).getWitness();
			
			//Get the Coin..
			CoinDBRow crow = getMainDB().getCoinDB().getCoinRow(coinid);
			if(crow == null) {
				InputHandler.endResponse(zMessage, false, "CoinID not found : "+coinid);
				return;
			}
			Coin cc = crow.getCoin();
			
			//Get the Script associated with this coin
			String script = getMainDB().getUserDB().getScript(cc.getAddress());
			if(script.equals("")) {
				InputHandler.endResponse(zMessage, false, "UNKNOWN ADDRESS "+cc.getAddress()+" not in database..");
				return;
			}
			
			//Add it..
			trx.addInput(cc);
			
			//Set Script
			wit.addScript(script);
			
			listTransactions(zMessage);
			
		}else if(zMessage.isMessageType(CONSENSUS_TXNOUTPUT)) {
			//Which transaction
			int trans    = zMessage.getInteger("transaction");
			
			//Check valid..
			if(!checkTransactionValid(trans)) {
				InputHandler.endResponse(zMessage, false, "Invalid TXN chosen : "+trans);
				return;
			}
			
			//Get the Address
			Address addr = (Address) zMessage.getObject("address");
			
			//And the Value
			String value = zMessage.getString("value");
			
			//What Token ID.. ?
			String tokenid = zMessage.getString("tokenid");
			
			//Create a coin
			Coin out = new Coin(Coin.COINID_OUTPUT,addr.getAddressData(),new MiniNumber(value), new MiniHash(tokenid));
			
			//Get the Transaction..
			Transaction trx = getMainDB().getUserDB().getUserRow(trans).getTransaction();
		
			//Add the output
			trx.addOutput(out);
			
			listTransactions(zMessage);
			
		}else if(zMessage.isMessageType(CONSENSUS_TXNSTATEVAR)) {
			//Which transaction
			int trans    		= zMessage.getInteger("transaction");
			int port     		= zMessage.getInteger("stateport");
			String variable		= zMessage.getString("statevariable");
			
			//Check valid..
			if(!checkTransactionValid(trans)) {
				InputHandler.endResponse(zMessage, false, "Invalid TXN chosen : "+trans);
				return;
			}
			
			//Get the Transaction..
			Transaction trx = getMainDB().getUserDB().getUserRow(trans).getTransaction();
		
			//Create a new State Variable
			StateVariable sv = new StateVariable(new MiniNumber(port+""), variable);
			
			//Add it to the transaction
			trx.addStateVariable(sv);
			
			listTransactions(zMessage);
			
		}else if(zMessage.isMessageType(CONSENSUS_TXNPOST)) {
			//Which transaction
			int trans    = zMessage.getInteger("transaction");
			
			//Check valid..
			if(!checkTransactionValid(trans)) {
				InputHandler.endResponse(zMessage, false, "Invalid TXN chosen : "+trans);
				return;
			}
			
			//Get the Transaction..
			Transaction trx =  getMainDB().getUserDB().getUserRow(trans).getTransaction();
			Witness wit     =  getMainDB().getUserDB().getUserRow(trans).getWitness();
			
			//Create the correct MMR Proofs
			Witness newwit = getMainDB().createValidWitness(trx, wit);
			if(newwit == null) {
				InputHandler.endResponse(zMessage, false, "ERROR creating valid Witness. MMR Proofs wrong..");
				return;
			}
			
			//Create the message
			Message msg = new Message(ConsensusHandler.CONSENSUS_SENDTRANS)
								.addObject("transaction", trx)
								.addObject("witness", wit);
			
			//Add the response message..
			InputHandler.addResponseMesage(msg, zMessage);
			
			//Post it..
			mHandler.PostMessage(msg);
			
		}else if(zMessage.isMessageType(CONSENSUS_TXNVALIDATE)) {
			//Which transaction
			int trans    = zMessage.getInteger("transaction");
			
			//Check valid..
			if(!checkTransactionValid(trans)) {
				InputHandler.endResponse(zMessage, false, "Invalid TXN chosen : "+trans);
				return;
			}
			
			//Get the user row
			UserDBRow row = getMainDB().getUserDB().getUserRow(trans);
			
			//Get the Transaction..
			Transaction trx =  row.getTransaction();
			Witness wit     = row.getWitness();
			
			//sum inputs
			JSONObject resp = InputHandler.getResponseJSON(zMessage);
			
			MiniNumber ins  = trx.sumInputs();
			MiniNumber outs = trx.sumOutputs();
			MiniNumber burn = ins.sub(outs);
					
			resp.put("inputs_sum", ins.toString());
			resp.put("outputs_sum", outs.toString());
			resp.put("burn", burn.toString());
			resp.put("valid_amounts", outs.isLessEqual(ins));
			
			//Create a complete transaction
			Witness newwit = getMainDB().createValidWitness(trx, wit);
			
			//Null valu means there is something wrong
			if(newwit == null) {
				resp.put("mmr_proof", false);
				resp.put("mmr_check", false);
				InputHandler.endResponse(zMessage, true, "");
				return;
			}else {
				resp.put("mmr_proof", true);
			}
			
			//And Check the actual Transaction..
			boolean checkok = TxPOWChecker.checkTransactionMMR(trx, wit, getMainDB(),
					getMainDB().getTopBlock(),
					getMainDB().getMainTree().getChainTip().getMMRSet(),false);
			
			resp.put("mmr_check", checkok);
			
			InputHandler.endResponse(zMessage, true, "");
			
		}else if(zMessage.isMessageType(CONSENSUS_TXNSIGN)) {
			//Sign the custom transaction
			int trans     = zMessage.getInteger("transaction");
			String pubkey = zMessage.getString("pubkey");
			MiniData pubk = new MiniData(pubkey);
			
			//Check valid..
			if(!checkTransactionValid(trans)) {
				InputHandler.endResponse(zMessage, false, "Invalid TXN chosen : "+trans);
				return;
			}
			
			//Get the public key
			PubPrivKey key = getMainDB().getUserDB().getPubPrivKey(pubk);
			if(key == null) {
				MinimaLogger.log("ERROR : invalidate key to sign with. Not present in DB. "+pubkey);
				return;
			}
			
			//Get the user row
			UserDBRow row = getMainDB().getUserDB().getUserRow(trans);
			
			//Get the Transaction..
			Transaction trx =  row.getTransaction();
			Witness wit     = row.getWitness();
			
			MiniHash transhash = Crypto.getInstance().hashObject(trx);
			
			MiniData signature = key.sign(transhash);
			
			//Now set the SIG.. 
			wit.addSignature(pubk, signature);
			
			listTransactions(zMessage);
		}
		
		
	}

}
