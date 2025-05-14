package org.minima.system.commands.txn;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.json.JSONObject;

public class txnclear extends Command {

	public txnclear() {
		super("txnclear","[id:] (scriptmmr:) (signatures:) - Clear ALL the Witness data");
	}
	
	@Override
	public String getFullHelp() {
		return "\ntxnclear\n"
				+ "\n"
				+ "Clear the Witness data - signatures, mmr proofs and script proofs.\n"
				+ "\n"
				+ "id:\n"
				+ "    The id of the transaction to clear.\n"
				+ "\n"
				+ "scriptmmr:\n"
				+ "    Clear the scripts and mmr (default : true).\n"
				+ "\n"
				+ "signatures:\n"
				+ "    Clear the signatures (default : true).\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "txnclear id:multisig\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"id", "scriptmmr", "signatures"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();

		TxnDB db = MinimaDB.getDB().getCustomTxnDB();
		
		String id 			= getParam("id");
		boolean scriptmmr 	= getBooleanParam("scriptmmr", true);
		boolean sigs 		= getBooleanParam("signatures", true);
		
		//Get the Transaction..
		TxnRow txnrow 	= db.getTransactionRow(getParam("id"));
		if(txnrow == null) {
			throw new CommandException("Transaction not found : "+id);
		}
		
		txnrow.getTransaction().clearIsMonotonic();
		if(scriptmmr) {
			txnrow.getWitness().clearCoinProofs();
			txnrow.getWitness().clearScriptProofs();
		}
		
		if(sigs) {
			txnrow.getWitness().clearSignatures();
		}
		
		JSONObject resp = new JSONObject();
		ret.put("response", txnrow.toJSON());
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new txnclear();
	}

}
