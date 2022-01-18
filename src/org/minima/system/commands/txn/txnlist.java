package org.minima.system.commands.txn;

import java.util.ArrayList;

import org.minima.system.commands.Command;
import org.minima.system.commands.txn.txndb.TxnDB;
import org.minima.system.commands.txn.txndb.TxnRow;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class txnlist extends Command {

	public txnlist() {
		super("txnlist","List current custom transactions");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();

		TxnDB db = TxnDB.getDB();
		
		//The transaction
		ArrayList<TxnRow> txns = db.listTxns();
		
		JSONArray arr = new JSONArray();
		for(TxnRow txnrow : txns) {
			arr.add(txnrow.toJSON());
		}
		
		JSONObject resp = new JSONObject();
		ret.put("response", arr);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new txnlist();
	}

}
