package org.minima.system.commands.base;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.userprefs.UserDB;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONObject;

public class magic extends Command {

	public magic() {
		super("magic","(kissvm:) (txpowsize:) (txnsperblock:) - Set the Magic numbers that define the Minima network overall capacity");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"kissvm","txpowsize","txnsperblock"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();

		JSONObject resp = new JSONObject();
		
		UserDB udb = MinimaDB.getDB().getUserDB();
		
		if(existsParam("kissvm")) {
			//Set this as your KISSVM opcodes..
			udb.setMagicDesiredKISSVM(getNumberParam("kissvm"));
		}
		
		if(existsParam("txpowsize")) {
			//Set this as your KISSVM opcodes..
			udb.setMagicMaxTxPoWSize(getNumberParam("txpowsize"));
		}
		
		if(existsParam("txnsperblock")) {
			//Set this as your KISSVM opcodes..
			udb.setMagicMaxTxns(getNumberParam("txnsperblock"));
		}
		
		//Get the Tip..
		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
		resp.put("lastblock", tip.getTxPoW().getMagic().toJSON());
		
		JSONObject desired = new JSONObject();
		desired.put("kissvm", udb.getMagicDesiredKISSVM());
		desired.put("txpowsize", udb.getMagicMaxTxPoWSize());
		desired.put("txnsperblock", udb.getMagicMaxTxns());
		resp.put("desired", desired);
		
		//Add balance..
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new magic();
	}

}
