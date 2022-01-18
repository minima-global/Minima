package org.minima.system.commands.txn;

import java.util.ArrayList;

import org.minima.objects.Coin;
import org.minima.objects.StateVariable;
import org.minima.objects.Transaction;
import org.minima.objects.TxPoW;
import org.minima.objects.Witness;
import org.minima.system.Main;
import org.minima.system.brains.TxPoWGenerator;
import org.minima.system.commands.Command;
import org.minima.system.commands.txn.txndb.TxnDB;
import org.minima.utils.json.JSONObject;

public class txnpost extends Command {

	public txnpost() {
		super("txnpost","[id:] - Post a transaction");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();

		TxnDB db = TxnDB.getDB();
		
		//The transaction
		String id 			= getParam("id");
		
		//Get the Transaction
		Transaction trans = db.getTransactionRow(id).getTransaction();
		
		//Create a Witness..
		Witness witness = txnutils.createWitness(trans);
		
		//Now create the TxPoW
		TxPoW txpow = TxPoWGenerator.generateTxPoW(trans, witness);
		
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
