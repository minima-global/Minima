package org.minima.system.commands.txn;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONObject;

public class txndelete extends Command {

	public txndelete() {
		super("txndelete","[id:] - Delete this custom transaction");
	}
	
	@Override
	public String getFullHelp() {
		return "\ntxndelete\n"
				+ "\n"
				+ "Delete a previously created custom transaction.\n"
				+ "\n"
				+ "id:\n"
				+ "    The id of the transaction to delete.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "txndelete id:multisig\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"id"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();

		TxnDB db = MinimaDB.getDB().getCustomTxnDB();
		
		String id 	= getParam("id");
		
		//Get the Transaction..
		boolean found = db.deleteTransaction(id);
		
		JSONObject resp = new JSONObject();
		if(found) {
			ret.put("response", "Deleted");	
		}else {
			ret.put("response", "Not found");
		}
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new txndelete();
	}

}
