package org.minima.system.commands.scripts;

import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.wallet.ScriptRow;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Address;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class scripts extends Command {

	public scripts() {
		super("scripts","(address:) - Search scripts / addresses");
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		//Get the wallet..
		Wallet wallet = MinimaDB.getDB().getWallet();
		
		//Is there an address
		String address = getParam("address","");
		if(address.toLowerCase().startsWith("mx")) {
			//Convert back to normal hex..
			try {
				address = Address.convertMinimaAddress(address).to0xString();
			}catch(IllegalArgumentException exc) {
				throw new CommandException(exc.toString());
			}
		}
		
		if(address.equals("")) {
			
			//Get all the custom scripts
			ArrayList<ScriptRow> allscripts = wallet.getAllAddresses();
			
			JSONArray arr = new JSONArray();
			for(ScriptRow kr : allscripts) {
				arr.add(kr.toJSON());
			}
				
			//Put the details in the response..
			ret.put("response", arr);
			
		}else {
			
			//Search for that address
			ScriptRow scrow = wallet.getScriptFromAddress(address);
			if(scrow == null) {
				throw new CommandException("Script with that address not found");
			}
			
			//Put the details in the response..
			ret.put("response", scrow.toJSON());
		}
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new scripts();
	}

}
