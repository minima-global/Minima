package org.minima.system.commands.txn;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.objects.base.MiniData;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.MiniFile;
import org.minima.utils.MiniFormat;
import org.minima.utils.json.JSONObject;

public class txnexport extends Command {

	public txnexport() {
		super("txnexport","[id:] (file:) - Export a transaction as HEX or to a file");
	}
	
	@Override
	public String getFullHelp() {
		return "\ntxnexport\n"
				+ "\n"
				+ "Export a transaction as HEX or to a file.\n"
				+ "\n"
				+ "The output can then be imported using 'txnimport' to another node. E.g. For signing.\n"
				+ "\n"
				+ "id:\n"
				+ "    The id of the transaction to export.\n"
				+ "\n"
				+ "file: (optional)\n"
				+ "    File name/path to export the transaction to, must use the .txn extension.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "txnexport id:simpletxn\n"
				+ "\n"
				+ "txnexport id:multisig file:multisig.txn\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"id","file"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();

		TxnDB db = MinimaDB.getDB().getCustomTxnDB();
		
		String id   = getParam("id");
		
		//Get the Transaction..
		TxnRow txnrow 	= db.getTransactionRow(getParam("id"));
		if(txnrow == null) {
			throw new CommandException("Transaction not found : "+id);
		}
		
		if(existsParam("file")) {
			//The File.. 
			String file = getParam("file");
			
			//Create the file
			File output = MiniFile.createBaseFile(file);
			if(output.exists()) {
				output.delete();
			}
			
			//Now export this to a file..
			MiniFile.writeObjectToFile(output, txnrow);
			
			JSONObject resp = new JSONObject();
			resp.put("file", output.getAbsolutePath());
			resp.put("size", MiniFormat.formatSize(output.length()));
			
			ret.put("response", resp);
		}else {
			
			//Output to HEX..
			MiniData dv = MiniData.getMiniDataVersion(txnrow);
			
			JSONObject resp = new JSONObject();
			resp.put("data", dv.to0xString());
			ret.put("response", resp);
		}
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new txnexport();
	}

}
