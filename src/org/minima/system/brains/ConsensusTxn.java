package org.minima.system.brains;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.util.ArrayList;
import java.util.Random;

import org.minima.database.MinimaDB;
import org.minima.database.coindb.CoinDBRow;
import org.minima.database.mmr.MMREntry;
import org.minima.database.mmr.MMRProof;
import org.minima.database.mmr.MMRSet;
import org.minima.database.userdb.UserDBRow;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.PubPrivKey;
import org.minima.objects.StateVariable;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.proofs.ScriptProof;
import org.minima.objects.proofs.SignatureProof;
import org.minima.objects.proofs.TokenProof;
import org.minima.system.input.InputHandler;
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
	
	public static final String CONSENSUS_TXNAUTO 			= CONSENSUS_PREFIX+"TXNAUTO";
	public static final String CONSENSUS_TXNAUTOSIGN 		= CONSENSUS_PREFIX+"TXNAUTOSIGN";
	
	public static final String CONSENSUS_TXNINPUT 			= CONSENSUS_PREFIX+"TXNINPUT";
	public static final String CONSENSUS_TXNOUTPUT 			= CONSENSUS_PREFIX+"TXNOUTPUT";
	
	public static final String CONSENSUS_TXNSTATEVAR 		= CONSENSUS_PREFIX+"TXNSTATEVAR";
	
	public static final String CONSENSUS_TXNSIGN 			= CONSENSUS_PREFIX+"TXNSIGN";
	public static final String CONSENSUS_TXNVALIDATE 		= CONSENSUS_PREFIX+"TXNVALIDATE";
	
	public static final String CONSENSUS_TXNSCRIPT 		    = CONSENSUS_PREFIX+"TXNSCRIPT";
	
	public static final String CONSENSUS_TXNPOST 			= CONSENSUS_PREFIX+"TXNPOST";
	
	public static final String CONSENSUS_TXNEXPORT 			= CONSENSUS_PREFIX+"TXNEXPORT";
	public static final String CONSENSUS_TXNIMPORT 			= CONSENSUS_PREFIX+"TXNIMPORT";
	
	public static final String CONSENSUS_REMOUTPUT 			= CONSENSUS_PREFIX+"REMOUTPUT";
	public static final String CONSENSUS_REMINPUT 			= CONSENSUS_PREFIX+"REMINPUT";
	
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
	
	private void outputTransaction(Message zMessage, int zTransaction) {
		UserDBRow row =  getMainDB().getUserDB().getUserRow(zTransaction);
		if(row == null) {
			InputHandler.endResponse(zMessage, false, "Transaction "+zTransaction+" not found..");
			return;
		}
		
		InputHandler.getResponseJSON(zMessage).put("transaction", row.toJSON());
		InputHandler.endResponse(zMessage, true, "");
		return;
	}
	
	public void processMessage(Message zMessage) throws Exception {
		
		/**
		 * Custom Transactions
		 */
		if(zMessage.isMessageType(CONSENSUS_TXNCREATE)) {
			//Get a new Random ID
			int id = new Random().nextInt();
			while(getMainDB().getUserDB().getUserRow(id)!=null) {
				id = new Random().nextInt();
			}
			
			//Now see if one is specified..
			if(zMessage.exists("id")) {
				id = zMessage.getInteger("id");
				
				//Delete if exists..
				getMainDB().getUserDB().deleteUserRow(id);
			}
			
			getMainDB().getUserDB().addUserRow(id);
			
			listTransactions(zMessage);
		
		}else if(zMessage.isMessageType(CONSENSUS_TXNDELETE)) {
			//Which transaction
			int trans    = zMessage.getInteger("transaction");
			
			getMainDB().getUserDB().deleteUserRow(trans);
			
			listTransactions(zMessage);
			
		}else if(zMessage.isMessageType(CONSENSUS_TXNLIST)) {
			if(zMessage.exists("transaction")) {
				int trans = zMessage.getInteger("transaction");
				
				//Get the Transaction..
				UserDBRow row =  getMainDB().getUserDB().getUserRow(trans);
				if(row == null) {
					InputHandler.endResponse(zMessage, false, "Transaction "+trans+" not found..");
					return;
				}
				
				InputHandler.getResponseJSON(zMessage).put("transaction", row.toJSON());
				InputHandler.endResponse(zMessage, true, "");
				return;
			}
			
			//List them all..
			JSONArray arr = new JSONArray();
			
			//get all the transactions..
			ArrayList<UserDBRow> rows = getMainDB().getUserDB().getAllRows();
			for(UserDBRow row : rows) {
				arr.add(row.toJSON());
			}
			
			InputHandler.getResponseJSON(zMessage).put("transactions", arr);
			InputHandler.endResponse(zMessage, true, "");
			
		}else if(zMessage.isMessageType(CONSENSUS_TXNSCRIPT)) {
			int trans 			= zMessage.getInteger("transaction");
			String script 	    = zMessage.getString("script");
			String proof        = zMessage.getString("proof");
			
			//Check valid..
			if(!checkTransactionValid(trans)) {
				InputHandler.endResponse(zMessage, false, "Invalid TXN chosen : "+trans);
				return;
			}
		
			//Get the Transaction..
			Witness wit     =  getMainDB().getUserDB().getUserRow(trans).getWitness();
			
			//Create it..
			ScriptProof sp = new ScriptProof(script, proof);
			
			//Add it to the Transaction..
			wit.addScript(sp);
		
			listTransactions(zMessage);
			
		}else if(zMessage.isMessageType(CONSENSUS_TXNAUTO)) {
			int trans 			= zMessage.getInteger("transaction");
			
			//Check valid..
			if(!checkTransactionValid(trans)) {
				InputHandler.endResponse(zMessage, false, "Invalid TXN chosen : "+trans);
				return;
			}
			
			String amount  = zMessage.getString("amount");
			String address = zMessage.getString("address");
			String tokenid = zMessage.getString("tokenid");
			
			//How much to who ?
			if(address.startsWith("0x")) {
				//It's a regular HASH address
				address = new MiniData(address).to0xString();
			}else if(address.startsWith("Mx")) {
				//It's a Minima Address!
				address = Address.convertMinimaAddress(address).to0xString();
			}
			
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
			}
			
			//Continue constructing the transaction - outputs don't need scripts
			Address recipient= new Address(new MiniData(address));
			
			//Blank address - check change is non-null
			Address change = new Address(); 
			if(!total.isEqual(sendamount)) {
				change = getMainDB().getUserDB().newSimpleAddress();
			}
			
			//May already have had some state variables set..
			Transaction trx =  getMainDB().getUserDB().getUserRow(trans).getTransaction();
			
			//Create the Transaction
			Message ret = getMainDB().createTransaction(sendamount, 
					recipient, change, confirmed, tok, changetok,null,trx);
			
			//Get the witness and add relevant info..
			Witness wit             = (Witness) ret.getObject("witness");
			Transaction transaction = (Transaction) ret.getObject("transaction");
			
			//Is this a token transaction
			if(tokendets != null) {
				//Get the token details..
				wit.addTokenDetails(tokendets);
			}
			
			//Now We gave a valid transaction and witness.!
			getMainDB().getUserDB().getUserRow(trans).setTransaction(transaction);
			getMainDB().getUserDB().getUserRow(trans).setWitness(wit);
			
			outputTransaction(zMessage,trans);
			
		}else if(zMessage.isMessageType(CONSENSUS_TXNAUTOSIGN)) {
			//Add input to a custom transaction
			int trans 			= zMessage.getInteger("transaction");
			
			//Check valid..
			if(!checkTransactionValid(trans)) {
				InputHandler.endResponse(zMessage, false, "Invalid TXN chosen : "+trans);
				return;
			}
			
			//Get the Transaction..
			Transaction trx =  getMainDB().getUserDB().getUserRow(trans).getTransaction();
			Witness wit     =  getMainDB().getUserDB().getUserRow(trans).getWitness();
			
			//Transction hash
			MiniData transhash = Crypto.getInstance().hashObject(trx);
			
			//Clear the previous signatures
			wit.clearSignatures();
			
			//Resign the inputs..
			ArrayList<Coin> inputs = trx.getAllInputs();
			for(Coin input : inputs){
				//Is it a simple..
				MiniData pubkey = getMainDB().getUserDB().getPublicKeyForSimpleAddress(input.getAddress());
				if(pubkey != null) {
					//Get the Pub Priv..
					PubPrivKey signer = getMainDB().getUserDB().getPubPrivKey(pubkey);
					
					//Sign the data
					MiniData signature = signer.sign(transhash);
					
					//Add to the witness..
					wit.addSignature(pubkey, signature);
				}
			}
			
			//List current..
			outputTransaction(zMessage,trans);
		
		}else if(zMessage.isMessageType(CONSENSUS_TXNINPUT)) {
			//Add input to a custom transaction
			int trans 			= zMessage.getInteger("transaction");
			MiniData coinid 	= (MiniData) zMessage.getObject("coinid");
			
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
			Coin cc        = null;
			
			//If it isn't one of OUR coins..
			if(crow==null) {
				//Get the MMRSet
				MMRSet basemmr = getMainDB().getMainTree().getChainTip().getMMRSet();
				
				//Search for the coin..
				MMREntry entry =  basemmr.findEntry(coinid);
				
				//Coin found..
				if(entry != null) {
					cc = entry.getData().getCoin();	
				}else {
					InputHandler.endResponse(zMessage, false, "CoinID not found : "+coinid);
					return;	
				}
			}else {
				cc = crow.getCoin();
			}
						
			//Is it a Token ? 
			if(!cc.getTokenID().isEqual(Coin.MINIMA_TOKENID)) {
				//Add the Token details..
				TokenProof tokendets = getMainDB().getUserDB().getTokenDetail(cc.getTokenID());
				
				//Do we have it,.
				if(tokendets == null) {
					//Unknown token!
					InputHandler.endResponse(zMessage, false, "No details found for the specified token : "+cc.getTokenID());
					return;
				}
				
				//Add it..
				wit.addTokenDetails(tokendets);
			}
			
			//Add it..
			if(zMessage.exists("position")) {
				int pos = zMessage.getInteger("position");
				trx.addInput(cc,pos);
			}else {
				trx.addInput(cc);	
			}
			
			//Get the Script associated with this coin
			String script = getMainDB().getUserDB().getScript(cc.getAddress());
			if(script.equals("")) {
				JSONObject resp = InputHandler.getResponseJSON(zMessage);
				resp.put("info", "UNKNOWN ADDRESS "+cc.getAddress()+" not in Script database..");
			}else {
				wit.addScript(script, cc.getAddress().getLength()*8);
			}
			
			outputTransaction(zMessage,trans);
			
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
			Coin out = new Coin(Coin.COINID_OUTPUT,addr.getAddressData(),new MiniNumber(value), new MiniData(tokenid));
			
			//Get the Transaction..
			Transaction trx = getMainDB().getUserDB().getUserRow(trans).getTransaction();
			Witness wit     = getMainDB().getUserDB().getUserRow(trans).getWitness();
			
			//Is it a Token ? 
			if(!out.getTokenID().isEqual(Coin.MINIMA_TOKENID)) {
				//Add the Token details..
				TokenProof tokendets = getMainDB().getUserDB().getTokenDetail(out.getTokenID());
				
				//Do we have it,.
				if(tokendets == null) {
					//Unknown token!
					InputHandler.endResponse(zMessage, false, "No details found for the specified token : "+out.getTokenID());
					return;
				}
				
				//Add it..
				wit.addTokenDetails(tokendets);
			}
			
			//Add it..
			if(zMessage.exists("position")) {
				int pos = zMessage.getInteger("position");
				trx.addOutput(out,pos);
			}else {
				trx.addOutput(out);
			}
			
			outputTransaction(zMessage,trans);
			
		}else if(zMessage.isMessageType(CONSENSUS_REMOUTPUT)) {
			int trans    	= zMessage.getInteger("transaction");
			int position    = zMessage.getInteger("position");
		
			Transaction trx = getMainDB().getUserDB().getUserRow(trans).getTransaction();
			trx.getAllOutputs().remove(position);
			
			outputTransaction(zMessage,trans);
			
		}else if(zMessage.isMessageType(CONSENSUS_REMINPUT)) {
			int trans    	= zMessage.getInteger("transaction");
			int position    = zMessage.getInteger("position");
		
			Transaction trx = getMainDB().getUserDB().getUserRow(trans).getTransaction();
			trx.getAllInputs().remove(position);
			
			outputTransaction(zMessage,trans);
			
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
			Witness wit     = getMainDB().getUserDB().getUserRow(trans).getWitness();
			
			//Create a new State Variable
			StateVariable sv = new StateVariable(port, variable);
			
			//Add it to the transaction
			trx.addStateVariable(sv);
			
			outputTransaction(zMessage,trans);
			
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
			Witness newwit = getMainDB().createValidMMRPRoofs(trx, wit);
			if(newwit == null) {
				InputHandler.endResponse(zMessage, false, "ERROR creating valid Witness. MMR Proofs wrong..");
				return;
			}
			
			//Check the INPUTS against the MEMPOOL COINS..
			if(getMainDB().checkTransactionForMempoolCoins(trx)) {
				//No GOOD!
				InputHandler.endResponse(zMessage, false, "ERROR double spend coin in mempool.");
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
			
			boolean vamounts = trx.checkValidInOutPerToken();
			
			resp.put("inputs_sum", ins.toString());
			resp.put("outputs_sum", outs.toString());
			resp.put("burn", burn.toString());
			resp.put("valid_amounts", vamounts);
			
			//Create a complete transaction
			Witness newwit = getMainDB().createValidMMRPRoofs(trx, wit);
			
			//Null value means there is something wrong
			if(newwit == null) {
				resp.put("mmr_proof", false);
			}else {
				resp.put("mmr_proof", true);
			}
			
			//And Check the actual Transaction..
			JSONArray contractlogs = new JSONArray();
			boolean checkok = TxPOWChecker.checkTransactionMMR(trx, wit, getMainDB(),
					getMainDB().getTopBlock(),
					getMainDB().getMainTree().getChainTip().getMMRSet(),false,contractlogs);
			
			resp.put("script_check", checkok);
			resp.put("contracts", contractlogs);
			
			//Check Signatures
			MiniData transhash = Crypto.getInstance().hashObject(trx);
			
			//Get all the signatures..
			ArrayList<SignatureProof> sigs = wit.getAllSignatures();
			
			//Check each one and add.. this is only done once..
			boolean sigsok = true;
			for(SignatureProof sig : sigs) {
				//This is the actual public key that is being represented..
//				MiniData leafkey = sig.getFinalHash();
				//Now check the leaf of the tree
				MiniData leafkey   = sig.getData();
				MiniData signature = sig.getSignature();
			
				//Check it..
				boolean ok = PubPrivKey.verify(leafkey, transhash, signature);
				if(!ok) {
					sigsok = false;
				}
			}
			resp.put("signatures", sigsok);
			
			//Final full check..
			resp.put("txnvalid", sigsok && vamounts && checkok && (newwit != null));
			
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
			
			MiniData transhash = Crypto.getInstance().hashObject(trx);
			
			MiniData signature = key.sign(transhash);
			
			//Now set the SIG.. 
			wit.addSignature(key.getPublicKey(), signature);
			
			outputTransaction(zMessage,trans);
			
		}else if(zMessage.isMessageType(CONSENSUS_TXNEXPORT)) {
			//Export the entire transaction as HEX data.. 
			int trans = zMessage.getInteger("transaction");
			
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
			
			//Create the Correct Proofs..
			getMainDB().createValidMMRPRoofs(trx, wit);
			
			//Output data stream
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			DataOutputStream dos = new DataOutputStream(baos);
			
			//Now the whole transaction..
			trx.writeDataStream(dos);
			
			//Now all the witness data
			wit.writeDataStream(dos);
			dos.flush();
			
			//Get the final Data..
			MiniData transdata = new MiniData(baos.toByteArray()); 
			
			JSONObject resp = InputHandler.getResponseJSON(zMessage);
			resp.put("transaction", transdata.to0xString());
			InputHandler.endResponse(zMessage, true, "");
			
			dos.close();
		
		}else if(zMessage.isMessageType(CONSENSUS_TXNIMPORT)) {
			//Import to this transaction 
			int trans = zMessage.getInteger("transaction");
			String data = zMessage.getString("data");
			MiniData md = new MiniData(data);
			
			//Delete if Exists
			getMainDB().getUserDB().deleteUserRow(trans);
			UserDBRow row =  getMainDB().getUserDB().addUserRow(trans);
			
			//Convert to a data stream
			ByteArrayInputStream bais = new ByteArrayInputStream(md.getData());
			DataInputStream dis = new DataInputStream(bais);
			
			//Now get the TRansaction
			row.getTransaction().readDataStream(dis);
			
			//And the Witness..
			row.getWitness().readDataStream(dis);
			
			//Import all the proofs..
			JSONObject resp = InputHandler.getResponseJSON(zMessage);
			for(MMRProof proof : row.getWitness().getAllMMRProofs()) {
				boolean valid = ConsensusUser.importCoin(getMainDB(), proof);
			
				if(!valid) {
					resp.put("error", "INVALID PROOF!");
					resp.put("proof", proof.toJSON());	
					InputHandler.endResponse(zMessage, true, "");
					
					return;
				}
			}
			
			listTransactions(zMessage);
		}
		
		
	}

}
