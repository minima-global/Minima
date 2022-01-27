package org.minima.system.commands.base;

import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.wallet.KeyRow;
import org.minima.database.wallet.Wallet;
import org.minima.kissvm.Contract;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class scripts extends Command {

	public scripts() {
		super("scripts","[action:list|newscript] (trackall:true|false) (script:) - Create a new custom script to track or list all scripts");
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		//Get the wallet..
		Wallet wallet = MinimaDB.getDB().getWallet();
		
		String action = getParam("action");
		
		if(action.equals("list")) {
			
			//Get all the custom scripts
			ArrayList<KeyRow> allscripts = wallet.getAllCustomScripts();
			
			JSONArray arr = new JSONArray();
			for(KeyRow kr : allscripts) {
				arr.add(kr.toJSON());
			}
				
			//Put the details in the response..
			ret.put("response", arr);
			
		}else {
			//Get the script
			String script = getParam("script");
			boolean track = getBooleanParam("trackall");
			
			//Clean the script
			script = Contract.cleanScript(script);
			
			//Now add it to the DB
			KeyRow krow = wallet.addScript(script,track);
			
			//Put the details in the response..
			ret.put("response", krow.toJSON());
		}
		
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new scripts();
	}

}
