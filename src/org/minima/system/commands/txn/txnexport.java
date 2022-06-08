package org.minima.system.commands.txn;

import java.io.File;

import org.minima.database.MinimaDB;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.MiniFile;
import org.minima.utils.MiniFormat;
import org.minima.utils.json.JSONObject;

public class txnexport extends Command {

	public txnexport() {
		super("txnexport","[id:] [file:] - Export a transaction to a file");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();

		TxnDB db = MinimaDB.getDB().getCustomTxnDB();
		
		String id   = getParam("id");
		String file = getParam("file");
		
		//Get the Transaction..
		TxnRow txnrow 	= db.getTransactionRow(getParam("id"));
		if(txnrow == null) {
			throw new CommandException("Transaction not found : "+id);
		}
		
		//Create the file
		File output = new File(file);
		if(output.exists()) {
			output.delete();
		}
		
		//Now export this to a file..
		MiniFile.writeObjectToFile(output, txnrow);
		
		JSONObject resp = new JSONObject();
		resp.put("file", output.getAbsolutePath());
		resp.put("size", MiniFormat.formatSize(output.length()));
		
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new txnexport();
	}

}
