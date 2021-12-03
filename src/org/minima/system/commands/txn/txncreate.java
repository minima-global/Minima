package org.minima.system.commands.txn;

import org.minima.objects.base.MiniData;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONObject;

public class txncreate extends Command {

	public txncreate() {
		super("txncreate","[id:] - create a transaction");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();

		TxnDB db = TxnDB.getDB();
		
		//The transaction
		String id = getParam("id");
		
		db.createTransaction(id);
		
		JSONObject resp = new JSONObject();
		resp.put("id", id);
		resp.put("transaction", db.getTransaction(id).toJSON());
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new txncreate();
	}

}
