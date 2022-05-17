package org.minima.system.commands.search;

import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.wallet.KeyRow;
import org.minima.database.wallet.Wallet;
import org.minima.objects.base.MiniData;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class keys extends Command {

	public keys() {
		super("keys","(private:true|false) (import:) - Get a list of all your public keys and addresses, show private keys or import");
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		boolean privkey = getBooleanParam("private",false); 
		
		//Get the wallet..
		Wallet wallet = MinimaDB.getDB().getWallet();
	
		if(existsParam("import")) {
			
			//Get the private seed
			MiniData privdata = getDataParam("import");
		
			//Create a new Key
			wallet.createNewKey(privdata, true);
		}
		
		//Get all the keys
		ArrayList<KeyRow> keys = wallet.getAllRelevant(false);
		
		JSONArray arr = new JSONArray();
		for(KeyRow kr : keys) {
			
			JSONObject dets = kr.toJSON();
			if(privkey) {
				dets.put("privatekey", kr.getPrivateKey());	
			}
			
			arr.add(dets);
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
