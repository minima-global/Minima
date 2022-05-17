package org.minima.system.commands.txn;

import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.objects.CoinProof;
import org.minima.objects.Transaction;
import org.minima.objects.TxPoW;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniNumber;
import org.minima.system.Main;
import org.minima.system.brains.TxPoWGenerator;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.json.JSONObject;

public class txnpost extends Command {

	public txnpost() {
		super("txnpost","[id:] (auto:true) (burn:) - Post a transaction. Automatically set the Scripts and MMR");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();

		TxnDB db = MinimaDB.getDB().getCustomTxnDB();
		
		//The transaction
		String id 		= getParam("id");
		MiniNumber burn = getNumberParam("burn", MiniNumber.ZERO);
		if(burn.isLess(MiniNumber.ZERO)) {
			throw new CommandException("Cannot have negative burn "+burn.toString());
		}
		
		//Get the row..
		TxnRow txnrow = db.getTransactionRow(id); 
		if(txnrow == null) {
			throw new CommandException("Transaction not found : "+id);
		}
		
		//Get the Transaction
		Transaction trans = txnrow.getTransaction();
		Witness wit		  = txnrow.getWitness();
		
		//Clear any previous checks..
		txnrow.getTransaction().clearIsMonotonic();
		
		//Set the scripts and MMR
		boolean auto = getBooleanParam("auto", false);
		if(auto) {
			//Set the MMR data and Scripts
			txnutils.setMMRandScripts(trans, wit);
		}
		
		//Compute the correct CoinID
		TxPoWGenerator.precomputeTransactionCoinID(trans);
		
		//Calculate the TransactionID..
		trans.calculateTransactionID();
		
		//The final TxPoW
		TxPoW txpow = null;
		
		//Is there a burn
		if(burn.isMore(MiniNumber.ZERO)) {
			
			//Get all the used coins..
			ArrayList<String> addedcoinid 	= new ArrayList<>();
			ArrayList<CoinProof> coins 		= wit.getAllCoinProofs();
			for(CoinProof cp : coins) {
				addedcoinid.add(cp.getCoin().getCoinID().to0xString());
			}
			
			//Create a Burn Transaction
			TxnRow burntxn = txnutils.createBurnTransaction(addedcoinid,trans.getTransactionID(),burn);

			//Now create a complete TxPOW
			txpow = TxPoWGenerator.generateTxPoW(trans, wit, burntxn.getTransaction(), burntxn.getWitness());
			
		}else {
			//Now create the TxPoW
			txpow = TxPoWGenerator.generateTxPoW(trans, wit);
		}
		
		//Calculate the size..
		txpow.calculateTXPOWID();
		
		//All good..
		ret.put("response", txpow.toJSON());
				
		//Send it to the Miner..
		Main.getInstance().getTxPoWMiner().mineTxPoW(txpow);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new txnpost();
	}

}
