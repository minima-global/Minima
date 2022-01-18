package org.minima.system.commands.all;

import org.minima.database.MinimaDB;
import org.minima.database.wallet.KeyRow;
import org.minima.database.wallet.Wallet;
import org.minima.kissvm.Contract;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.json.JSONObject;

public class newscript extends Command {

	public newscript() {
		super("newscript","[script:] - Create a new custom script to track");
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		//Get the wallet..
		Wallet wallet = MinimaDB.getDB().getWallet();
		
		//Get the script
		String script = getParam("script");
		
		//Clean the script
		script = Contract.cleanScript(script);
		
		//Now add it to the DB
		KeyRow krow = wallet.addScript(script);
		
		//Put the details in the response..
		ret.put("response", krow.toJSON());
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new newscript();
	}

}
