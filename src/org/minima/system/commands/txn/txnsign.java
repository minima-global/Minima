package org.minima.system.commands.txn;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.database.wallet.KeyRow;
import org.minima.database.wallet.ScriptRow;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Coin;
import org.minima.objects.Transaction;
import org.minima.objects.TxPoW;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.keys.Signature;
import org.minima.system.brains.TxPoWGenerator;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.commands.backup.vault;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class txnsign extends Command {

	public txnsign() {
		super("txnsign","[id:] [publickey:0x..|auto] (txnpostauto:) (txnpostburn:) (txnpostmine:) - Sign a transaction");
	}
	
	@Override
	public String getFullHelp() {
		return "\ntxnsign\n"
				+ "\n"
				+ "Sign a transaction.\n"
				+ "\n"
				+ "Specify the public key or use 'auto' if the coin inputs are simple.\n"
				+ "\n"
				+ "id:\n"
				+ "    The id of the transaction to sign.\n"
				+ "\n"
				+ "publickey:\n"
				+ "    The public key specified in a custom script, or 'auto' for transactions with simple inputs.\n"
				+ "\n"
				+ "txnpostauto: (optional)\n"
				+ "    Do you want to post this transaction. Use the same values as you would for txnpost auto(sort MMR and Scripts)\n"
				+ "\n"
				+ "txnpostburn: (optional)\n"
				+ "    If you also post this transaction, do you want to add a burn transaction.\n"
				+ "\n"
				+ "txnpostmine: (optional)\n"
				+ "    If you also post this transaction, do you want to mine it immediately.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "txnsign id:simpletxn publickey:auto\n"
				+ "\n"
				+ "txnsign id:simpletxn publickey:auto password:your_password\n"
				+ "\n"
				+ "txnsign id:multisig publickey:0xFD8B..\n"
				+ "\n"
				+ "txnsign id:simpletxn publickey:auto txnpostauto:true\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"id","publickey","txnpostauto","txnpostburn","txnpostmine","password"}));
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
		
		boolean passwordlock = false;
		if(existsParam("password") && !MinimaDB.getDB().getWallet().isBaseSeedAvailable()) {
			
			//Lets unlock the DB
			vault.passowrdUnlockDB(getParam("password"));
			 
			//Lock at the end..
			passwordlock = true;
		}
		
		//Are we auto signing.. if all the coin inputs are simple
		if(pubk.equals("auto")) {
			
			ArrayList<Coin> inputs = txn.getAllInputs();
			for(Coin cc : inputs) {
				
				//Get the Public Key for this address if possible
				ScriptRow scrow = walletdb.getScriptFromAddress(cc.getAddress().to0xString());
				if(scrow == null) {
					notfoundkeys.add(cc.getAddress().to0xString());
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
				
				//Are we locking the DB
				if(passwordlock) {
					//Lock the Wallet DB
					vault.passwordLockDB(getParam("password"));
				}
				
				throw new CommandException("Public Key not found : "+pubk);
			}
			
			//Add to our list
			foundkeys.add(pubrow.getPublicKey());
			
			//Use the wallet..
			Signature signature = walletdb.signData(pubrow.getPublicKey(), txn.getTransactionID());
				
			//Add it..
			wit.addSignature(signature);
		}
		
		//Are we locking the DB
		if(passwordlock) {
			//Lock the Wallet DB
			vault.passwordLockDB(getParam("password"));
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
		
		//Are we auto posting ASWELL..
		if(existsParam("txnpostauto")) {
			
			//Are we AUTO
			boolean postauto 	= getBooleanParam("txnpostauto");
			
			//Get the burn
			MiniNumber burn 	= getNumberParam("txnpostburn", MiniNumber.ZERO);
			
			//Are we Mining synchronously
			boolean minesync 	= getBooleanParam("txnpostmine", false);
			
			//And POst it..
			TxPoW txp 			= txnpost.postTxn(id, burn, postauto,minesync);
			
			resp.put("txnpost", true);
			resp.put("txnpostauto", postauto);
			resp.put("txnpostburn", burn.toString());
			resp.put("txnpostmine", minesync);
			resp.put("txpow", txp.toJSON());
			
		}else {
			resp.put("txnpost", false);
		}
		
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new txnsign();
	}

}
