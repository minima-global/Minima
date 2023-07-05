package org.minima.system.commands.search;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.wallet.KeyRow;
import org.minima.database.wallet.Wallet;
import org.minima.objects.base.MiniData;
import org.minima.objects.keys.TreeKey;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class keys extends Command {

	public keys() {
		super("keys","(action:list|new|checkkeys) (publickey:) - Get a list of all your public keys or create a new key");
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
				+ "    checkkeys : Checks if your Public and Private keys are correct.\n"
				+ "    new : Create a new key pair.\n"
				+ "\n"
				+ "publickey: (optional)\n"
				+ "    Search for a specific public key.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "keys\n"
				+ "\n"
				+ "keys action:list\n"
				+ "\n"
				+ "keys action:checkkeys\n"
				+ "\n"
				+ "keys action:list publickey:0xFFEE56..\n"
				+ "\n"
				+ "keys action:new\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","publickey"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		//Get the wallet..
		Wallet wallet = MinimaDB.getDB().getWallet();
		
		String action = getParam("action", "list");
		
		if(action.equals("list")) {
			
			//Are we searching for a psecific key
			boolean searchkey 	= false;
			String pubkey 		= getParam("publickey", "");
			if(!pubkey.equals("")) {
				searchkey = true;
			}
			
			//Get all the keys
			ArrayList<KeyRow> keys = wallet.getAllKeys();
			
			JSONArray arr 	= new JSONArray();
			int maxuses		= 0;
			for(KeyRow kr : keys) {
				
				//Get the details
				JSONObject dets = kr.toJSON();
				
				//Are we seraching for ONE key or ALL of them
				if(searchkey) {
					if(dets.getString("publickey").equals(pubkey)) {
						if(kr.getUses()>maxuses) {
							maxuses = kr.getUses(); 
						}
						
						arr.add(dets);
						
						break;
					}
				}else {
					if(kr.getUses()>maxuses) {
						maxuses = kr.getUses(); 
					}
					
					arr.add(dets);
				}
			}
				
			JSONObject resp = new JSONObject();
			resp.put("keys", arr);
			resp.put("total", arr.size());
			resp.put("maxuses", maxuses);
			
			//Put the details in the response..
			ret.put("response", resp);
			
		}else if(action.equals("checkkeys")) {
			
			//Only unlocked will work
			if(!MinimaDB.getDB().getWallet().isBaseSeedAvailable()) {
				throw new CommandException("Cannot check keys of locked DB..");
			}
			
			//Get all the keys
			ArrayList<KeyRow> keys = wallet.getAllKeys();
			
			int correct		= 0;
			int wrong		= 0;
					
			for(KeyRow kr : keys) {
				TreeKey tk = new TreeKey( new MiniData(kr.getPrivateKey()), kr.getSize(), kr.getDepth());
				MiniData pubk 		= new MiniData(kr.getPublicKey());
				MiniData actualkey 	= tk.getPublicKey();
				if(!pubk.isEqual(actualkey)) {
					MinimaLogger.log("[!] INCORRECT Public key : "+pubk+" / "+actualkey);
					wrong++;
				}else {
					MinimaLogger.log("CORRECT Public key : "+pubk);
					correct++;
				}
			}
				
			JSONObject resp = new JSONObject();
			resp.put("allkeys", keys.size());
			resp.put("correct", correct);
			resp.put("wrong", wrong);
			
			//Put the details in the response..
			ret.put("response", resp);
			
		}else if(action.equals("new")) {
			
			//Create a new Key..
			KeyRow krow = wallet.createNewKey();
			ret.put("response", krow.toJSON());
			
		}else {
			throw new CommandException("Unknown action : "+action);
		}
		
		return ret;
	}

	public static boolean checkKey(String zPublicKey) {
		MiniData pubkey = new MiniData(zPublicKey);
		KeyRow kr 		= MinimaDB.getDB().getWallet().getKeyFromPublic(zPublicKey);
		
		//Check this keys private key is correct - hash(Seed+Mod)
		if(!MinimaDB.getDB().getWallet().checkSingleKey(kr.getPrivateKey(), kr.getModifier())) {
			return false;
		}
		
		//Check the address
		TreeKey tk = new TreeKey( new MiniData(kr.getPrivateKey()), kr.getSize(), kr.getDepth());
		MiniData actualkey 	= tk.getPublicKey();
		if(!pubkey.isEqual(actualkey)) {
			return false;
		}
		
		return true;
	}
	
	@Override
	public Command getFunction() {
		return new keys();
	}

}
