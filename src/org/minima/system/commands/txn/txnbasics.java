package org.minima.system.commands.txn;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.system.brains.TxPoWGenerator;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.json.JSONObject;

public class txnbasics extends Command {

	public txnbasics() {
		super("txnbasics","[id:] - Automatically set the MMR proofs and scripts for a txn");
	}
	
	@Override
	public String getFullHelp() {
		return "\ntxnbasics\n"
				+ "\n"
				+ "Automatically set the MMR proofs and scripts for a transaction.\n"
				+ "\n"
				+ "Only run this when a transaction is ready to be posted.\n"
				+ "\n"
				+ "id:\n"
				+ "    The id of the transaction.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "txnbasics id:simpletxn\n";
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
		String id 		= getParam("id");
		TxnRow txnrow 	= db.getTransactionRow(id); 
		if(txnrow == null) {
			throw new CommandException("Transaction not found : "+id);
		}
		
		//Get the Transaction
		Transaction trans = txnrow.getTransaction();
		Witness wit		  = txnrow.getWitness();
		
		//Set the MMR data and Scripts - for coins you have have
		txnutils.setMMRandScripts(trans, wit, false);
		
		//Compute the correct CoinID
		TxPoWGenerator.precomputeTransactionCoinID(trans);
		
		//Calculate the TransactionID..
		trans.calculateTransactionID();
				
		//All good..
		ret.put("response", txnrow.toJSON());
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new txnbasics();
	}

}
