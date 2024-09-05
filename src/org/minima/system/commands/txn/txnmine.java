package org.minima.system.commands.txn;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMRProof;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.objects.Coin;
import org.minima.objects.CoinProof;
import org.minima.objects.Transaction;
import org.minima.objects.TxPoW;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.Main;
import org.minima.system.brains.TxPoWGenerator;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.params.GlobalParams;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;

public class txnmine extends Command {

	public txnmine() {
		super("txnmine","[id:] - Mine a txn but don't post it");
	}
	
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"id"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();

		TxnDB db = MinimaDB.getDB().getCustomTxnDB();
		
		//The transaction
		String id 			= getParam("id");
		
		//Get the Transaction
		TxnRow txnrow 	= db.getTransactionRow(getParam("id"));
		if(txnrow == null) {
			throw new CommandException("Transaction not found : "+id);
		}
		
		//Clear any previous checks..
		txnrow.getTransaction().clearIsMonotonic();
		
		//Get the txn..
		Transaction trans 	= txnrow.getTransaction();
		Witness wit		  	= txnrow.getWitness();
		
		//Compute the correct CoinID
		TxPoWGenerator.precomputeTransactionCoinID(trans);
		
		//Calculate the TransactionID..
		trans.calculateTransactionID();
		
		//Now create the TxPoW
		TxPoW txpow = TxPoWGenerator.generateTxPoW(trans, wit);
		
		//Calculate the size..
		txpow.calculateTXPOWID();
		
		//Now Mine it BUT dont POST it..
		boolean success = Main.getInstance().getTxPoWMiner().MineMaxTxPoW(false, txpow, 120000, false);
		
		if(!success) {
			throw new CommandException("FAILED TO MINE txn in 120 seconds !?");
		}
		
		//Now convert to DATA
		MiniData txdata = MiniData.getMiniDataVersion(txpow);
		
		//Return the MINED txn..
		JSONObject resp = new JSONObject();
		resp.put("txpowid", txpow.getTxPoWID());
		resp.put("data", txdata.to0xString());
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new txnmine();
	}

}
