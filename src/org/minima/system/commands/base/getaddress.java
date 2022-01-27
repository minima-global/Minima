package org.minima.system.commands.base;

import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.wallet.KeyRow;
import org.minima.database.wallet.Wallet;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class getaddress extends Command {

	public getaddress() {
		super("getaddress","(type:single|list) - Get a Minima address to receive funds");
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		//Get the wallet..
		Wallet wallet = MinimaDB.getDB().getWallet();
		
		String type = getParam("type","single");
		
		if(type.equals("single")) {
			//Get an existing address
			KeyRow krow = wallet.getKey();
				
			//Put the details in the response..
			ret.put("response", krow.toJSON());
			
		}else {
			
			//Get all the keys
			ArrayList<KeyRow> allkeys = wallet.getAllRelevant();
			
			//Add them to an array
			JSONArray keyarray =new JSONArray();
			for(KeyRow key : allkeys) {
				keyarray.add(key.toJSON());
			}
			
			//Put the details in the response..
			ret.put("response", keyarray);
		}
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new getaddress();
	}

}
