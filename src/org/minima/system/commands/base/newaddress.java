package org.minima.system.commands.base;

import org.minima.database.MinimaDB;
import org.minima.database.wallet.KeyRow;
import org.minima.database.wallet.Wallet;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONObject;

public class newaddress extends Command {

	public newaddress() {
		super("newaddress","Create a new address or public key");
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		//Get the wallet..
		Wallet wallet = MinimaDB.getDB().getWallet();
		
		//Create a new address
		KeyRow krow = wallet.createNewKey();
			
		//Put the details in the response..
		ret.put("response", krow.toJSON());
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new newaddress();
	}

}
