package org.minima.system.commands.txn;

import org.minima.database.MinimaDB;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONObject;

public class txndelete extends Command {

	public txndelete() {
		super("txndelete","[id:] - Delete this custom transaction");
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
