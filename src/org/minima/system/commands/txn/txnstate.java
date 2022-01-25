package org.minima.system.commands.txn;

import org.minima.database.MinimaDB;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.objects.StateVariable;
import org.minima.objects.Transaction;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONObject;

public class txnstate extends Command {

	public txnstate() {
		super("txnstate","[id:] [port:] [value:] (keeper:) - Add a state variable");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();

		TxnDB db = MinimaDB.getDB().getCustomTxnDB();
		
		//The transaction
		String id 			= getParam("id");
		String port			= getParam("port");
		String value		= getParam("value");
		boolean keeper	 	= true;
		if(existsParam("keeper")) {
			keeper = getBooleanParam("keeper");
		}
		
		//Get the Transaction
		Transaction trans = db.getTransactionRow(id).getTransaction();
		
		//Create a state variable..
		StateVariable sv = new StateVariable(Integer.parseInt(port),value,keeper);
		
		//Add it to the transaction
		trans.addStateVariable(sv);
		
		//Output the current trans..
		ret.put("response", db.getTransactionRow(id).toJSON());
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new txnstate();
	}

}
