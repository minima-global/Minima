package org.minima.system.commands.txn;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class txnlist extends Command {

	public txnlist() {
		super("txnlist","(id:) - List current custom transactions");
	}
	
	@Override
	public String getFullHelp() {
		return "\ntxnlist\n"
				+ "\n"
				+ "List your custom transactions. Includes previously posted transactions.\n"
				+ "\n"
				+ "Returns the full details of transactions.\n"
				+ "\n"
				+ "id: (optional)\n"
				+ "    The id of a single transaction to list.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "txnlist\n"
				+ "\n"
				+ "txnlist id:multisig\n";
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
		String id = getParam("id","");
		
		if(id.equals("")) {
			//The transaction
			ArrayList<TxnRow> txns = db.listTxns();
			
			JSONArray arr = new JSONArray();
			for(TxnRow txnrow : txns) {
				arr.add(txnrow.toJSON());
			}
			
			ret.put("response", arr);
		}else {
			TxnRow txnrow 	= db.getTransactionRow(getParam("id"));
			if(txnrow == null) {
				throw new CommandException("Transaction not found : "+id);
			}
			
			ret.put("response", txnrow.toJSON());
		}
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new txnlist();
	}

}
