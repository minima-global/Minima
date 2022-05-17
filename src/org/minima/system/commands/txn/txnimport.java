package org.minima.system.commands.txn;

import java.io.File;

import org.minima.database.MinimaDB;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.objects.base.MiniData;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.MiniFile;
import org.minima.utils.json.JSONObject;

public class txnimport extends Command {

	public txnimport() {
		super("txnimport","(id:) (file:) (data:) - Import a transaction as a file or HEX data. Optionally specify the ID");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();

		TxnDB db = MinimaDB.getDB().getCustomTxnDB();
		
		if(existsParam("file")) {
			String file = getParam("file");
			File ff = new File(file);
			if(!ff.exists()) {
				throw new CommandException("File does not exist : "+ff.getAbsolutePath());
			}
			
			//Load it in..
			byte[] txndata = MiniFile.readCompleteFile(ff);
			
			//Convert to MiniData
			MiniData minitxn = new MiniData(txndata);
			
			//Convert this..
			TxnRow txnrow = TxnRow.convertMiniDataVersion(minitxn);
			if(existsParam("id")) {
				txnrow.setID(getParam("id"));
			}
			
			db.addCompleteTransaction(txnrow);
			
			JSONObject resp = new JSONObject();
			ret.put("response", txnrow.toJSON());
			
		}else if(existsParam("data")){
			
			//Get the HEX data
			MiniData dv = getDataParam("data");
			
			//Convert to a TxnRow
			TxnRow tx 	= TxnRow.convertMiniDataVersion(dv);
			if(existsParam("id")) {
				tx.setID(getParam("id"));
			}
			
			//Add to the DB
			db.addCompleteTransaction(tx);
			
			JSONObject resp = new JSONObject();
			ret.put("response", tx.toJSON());
			
		}else {
			throw new CommandException("Must specify file or data");
		}
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new txnimport();
	}

}
