package org.minima.system.commands.txn;

import org.minima.database.MinimaDB;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.objects.base.MiniData;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.json.JSONObject;

public class txnexport extends Command {

	public txnexport() {
		super("txnexport","[id:] - Export a transaction");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();

		TxnDB db = MinimaDB.getDB().getCustomTxnDB();
		
		String id = getParam("id");
		
		//Get the Transaction..
		TxnRow txnrow 	= db.getTransactionRow(getParam("id"));
		if(txnrow == null) {
			throw new CommandException("Transaction not found : "+id);
		}
		
		//Export it..
		MiniData data = MiniData.getMiniDataVersion(txnrow);
		
		JSONObject resp = new JSONObject();
		ret.put("response", data.to0xString());
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new txnexport();
	}

}
