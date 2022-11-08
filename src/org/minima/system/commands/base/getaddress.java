package org.minima.system.commands.base;

import org.minima.database.MinimaDB;
import org.minima.database.wallet.ScriptRow;
import org.minima.database.wallet.Wallet;
import org.minima.system.commands.Command;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;

public class getaddress extends Command {

	public getaddress() {
		super("getaddress","[createall:true] Get one of your default Minima addresses");
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		//Get the wallet..
		Wallet wallet = MinimaDB.getDB().getWallet();
		
		//Are we creating them all..
		if(existsParam("createall")) {
			
			MinimaLogger.log("Creating all remaining keys..");
			
			//Create all remaining addresses..
			wallet.initDefaultKeys(Wallet.NUMBER_GETADDRESS_KEYS, true);
			
			ret.put("response", "All keys created..");
			
		}else {
		
			String type = getParam("type","single");
			
			//Get an existing address
			ScriptRow scrow = wallet.getDefaultAddress();
				
			//Put the details in the response..
			ret.put("response", scrow.toJSON());
		}
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new getaddress();
	}

}
