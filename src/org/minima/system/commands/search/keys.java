package org.minima.system.commands.search;

import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.wallet.KeyRow;
import org.minima.database.wallet.Wallet;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class keys extends Command {

	public keys() {
		super("keys","Get a list of all your public keys and addresses");
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		//Get the wallet..
		Wallet wallet = MinimaDB.getDB().getWallet();
		
		//Get all the keys
		ArrayList<KeyRow> keys = wallet.getAllRelevant();
		
		JSONArray arr = new JSONArray();
		for(KeyRow kr : keys) {
			arr.add(kr.toJSON());
		}
			
		//Put the details in the response..
		ret.put("response", arr);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new keys();
	}

}
