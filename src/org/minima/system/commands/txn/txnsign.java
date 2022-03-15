package org.minima.system.commands.txn;

import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.database.wallet.KeyRow;
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
		
		JSONArray foundkeys = new JSONArray();
		//Are we auto signing.. if all the coin inputs are simple
		if(pubk.equals("auto")) {
			
			ArrayList<Coin> inputs = txn.getAllInputs();
			for(Coin cc : inputs) {
				
				KeyRow keyrow = walletdb.getKeysRowFromAddress(cc.getAddress().to0xString()); 
				if(keyrow == null) {
					continue;

					//Is it a simple row..
				}else if(keyrow.getPublicKey().equals("")) {
					continue;
				}
				
				//Add to our list
				foundkeys.add(keyrow.getPublicKey());
				
				//Now sign with that..
				Signature signature = walletdb.sign(keyrow.getPrivateKey(), txn.getTransactionID());
					
				//Add it..
				wit.addSignature(signature);
			}
			
		}else {
			//Get the Private key..
			KeyRow pubrow 	= walletdb.getKeysRowFromPublicKey(pubk);
			if(pubrow == null) {
				throw new CommandException("Public Key not found : "+pubk);
			}
			
			foundkeys.add(pubrow.getPublicKey());
			
			//Use the wallet..
			Signature signature = walletdb.sign(pubrow.getPrivateKey(), txn.getTransactionID());
				
			//Add it..
			wit.addSignature(signature);
		}
		
		JSONObject resp = new JSONObject();
		resp.put("keys", foundkeys);
		
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new txnsign();
	}

}
