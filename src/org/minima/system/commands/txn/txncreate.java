package org.minima.system.commands.txn;

import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.commands.txn.txndb.TxnDB;
import org.minima.utils.json.JSONObject;

public class txncreate extends Command {

	public txncreate() {
		super("txncreate","[id:] - Create a transaction");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		
		TxnDB db = TxnDB.getDB();
		
		//The transaction
		String id = getParam("id");
		
		if(db.getTransactionRow(id) != null) {
			throw new CommandException("Txn with this ID already exists : "+id);
		}
		
		db.createTransaction(id);
		
		JSONObject ret = getJSONReply();
		ret.put("response", db.getTransactionRow(id).toJSON());
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new txncreate();
	}

}
