package org.minima.system.commands.search;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.wallet.KeyRow;
import org.minima.database.wallet.Wallet;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class keys extends Command {

	public keys() {
		super("keys","(action:list|new) - Get a list of all your public keys or create a new key");
	}
	
	@Override
	public String getFullHelp() {
		return "\nkeys\n"
				+ "\n"
				+ "Get a list of all your public keys or create a new key.\n"
				+ "\n"
				+ "Each public key can be used for signing securely 262144 (64^3) times.\n"
				+ "\n"
				+ "action: (optional)\n"
				+ "    list : List your existing public keys. The default.\n"
				+ "    new : Create a new key pair.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "keys\n"
				+ "\n"
				+ "keys action:list\n"
				+ "\n"
				+ "keys action:new\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		//Get the wallet..
		Wallet wallet = MinimaDB.getDB().getWallet();
		
		String action = getParam("action", "list");
		
		if(action.equals("list")) {
			
			//Get all the keys
			ArrayList<KeyRow> keys = wallet.getAllKeys();
			
			JSONArray arr = new JSONArray();
			for(KeyRow kr : keys) {
				JSONObject dets = kr.toJSON();
				arr.add(dets);
			}
				
			//Put the details in the response..
			ret.put("response", arr);
			
		}else if(action.equals("new")) {
			
			//Create a new Key..
			KeyRow krow = wallet.createNewKey();
			ret.put("response", krow.toJSON());
			
		}else {
			throw new CommandException("Unknown action : "+action);
		}
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new keys();
	}

}
