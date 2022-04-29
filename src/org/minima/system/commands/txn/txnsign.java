package org.minima.system.commands.txn;

import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.database.wallet.KeyRow;
import org.minima.database.wallet.ScriptRow;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Coin;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.objects.keys.Signature;
import org.minima.system.brains.TxPoWGenerator;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class txnsign extends Command {

	public txnsign() {
		super("txnsign","[id:] [publickey:0x..|auto] - Sign a transaction");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();

		TxnDB db = MinimaDB.getDB().getCustomTxnDB();
		
		String id 	= getParam("id");
		String pubk	= getParam("publickey");
		
		//Get the Transaction..
		TxnRow txnrow 	= db.getTransactionRow(getParam("id"));
		if(txnrow == null) {
			throw new CommandException("Transaction not found : "+id);
		}
		
		Transaction txn = txnrow.getTransaction();
		Witness wit		= txnrow.getWitness();
		
		//Precompute the CoinID
		TxPoWGenerator.precomputeTransactionCoinID(txn);
		
		//Calculate the TransactionID..
		txn.calculateTransactionID();
		
		//Get the Wallet
		Wallet walletdb = MinimaDB.getDB().getWallet();
		
		JSONArray notfoundkeys 		= new JSONArray();
		JSONArray foundkeys 		= new JSONArray();
		JSONArray nonsimplekeys 	= new JSONArray();
		
		JSONObject resp = new JSONObject();
		
		//Are we auto signing.. if all the coin inputs are simple
		if(pubk.equals("auto")) {
			
			ArrayList<Coin> inputs = txn.getAllInputs();
			for(Coin cc : inputs) {
				
				//Get the Public Key for this address if possible
				ScriptRow scrow = walletdb.getScriptFromAddress(cc.getAddress().to0xString());
				if(scrow == null) {
					notfoundkeys.add(scrow.getAddress());
					continue;
				}else if(!scrow.isSimple()) {
					nonsimplekeys.add(scrow.getAddress());
					continue;
				}
				
				//Don't try again if already signed..
				if(!wit.isSignedBy(scrow.getPublicKey())) {
					//Add to our list
					foundkeys.add(scrow.getPublicKey());
					
					//Now sign with that..
					Signature signature = walletdb.signData(scrow.getPublicKey(), txn.getTransactionID());
						
					//Add it..
					wit.addSignature(signature);
				}
			}
			
		}else {
			//Check we have it
			KeyRow pubrow = walletdb.getKeyFromPublic(pubk);
			if(pubrow == null) {
				throw new CommandException("Public Key not found : "+pubk);
			}
			
			//Add to our list
			foundkeys.add(pubrow.getPublicKey());
			
			//Use the wallet..
			Signature signature = walletdb.signData(pubrow.getPublicKey(), txn.getTransactionID());
				
			//Add it..
			wit.addSignature(signature);
		}
		
		//The keys that were found and used
		resp.put("keys", foundkeys);
	
		//Did we find any that were not simple
		if(nonsimplekeys.size()>0) {
			resp.put("nonsimple", nonsimplekeys);
		}
		
		//Did we not find any
		if(notfoundkeys.size()>0) {
			resp.put("notfound", notfoundkeys);
		}
		
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new txnsign();
	}

}
