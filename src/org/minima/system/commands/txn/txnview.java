package org.minima.system.commands.txn;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.objects.base.MiniData;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.MiniFile;
import org.minima.utils.json.JSONObject;

public class txnview extends Command {

	public txnview() {
		super("txnview","(file:) (data:) - View a transaction as a JSON.");
	}
	
	@Override
	public String getFullHelp() {
		return "\ntxnimport\n"
				+ "\n"
				+ "View a transaction from previously exported HEX data or a .txn file.\n"
				+ "\n"
				+ "file: (optional)\n"
				+ "    File name/path to the previously exported .txn file.\n"
				+ "\n"
				+ "data: (optional)\n"
				+ "    HEX data of the previously exported transaction.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "txnview data:0x0000..\n"
				+ "\n"
				+ "txnview file:multisig.txn\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"file","data"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();

		if(existsParam("file")) {
			String file = getParam("file");
			File ff = MiniFile.createBaseFile(file);
			if(!ff.exists()) {
				throw new CommandException("File does not exist : "+ff.getAbsolutePath());
			}
			
			//Load it in..
			byte[] txndata = MiniFile.readCompleteFile(ff);
			
			//Convert to MiniData
			MiniData minitxn = new MiniData(txndata);
			
			//Convert this..
			TxnRow txnrow = TxnRow.convertMiniDataVersion(minitxn);
			
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
			
			JSONObject resp = new JSONObject();
			ret.put("response", tx.toJSON());
			
		}else {
			throw new CommandException("Must specify file or data");
		}
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new txnview();
	}

}
