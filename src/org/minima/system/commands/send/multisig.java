package org.minima.system.commands.send;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.objects.base.MiniNumber;
import org.minima.system.commands.Command;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class multisig extends Command {

	public multisig() {
		super("multisig","");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","required","publickeys"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
	
		//What are we doing..
		String action = getParam("action");
		
		if(action.equals("create")) {
			
			//How many required
			MiniNumber required = getNumberParam("required");
			
			//Now create the script..
			String script = "RETURN MULTISIG("+required;
			
			//Get the pub keys
			ArrayList<String> allkeys = new ArrayList<>();
			JSONArray pubkeys = getJSONArrayParam("publickeys");
			for(Object obj : pubkeys) {
				String pubkey = (String)obj;
				allkeys.add(pubkey);
				
				script+=" "+pubkey;
			}
			script+=")";
			
			//Now add this script..
			
			MinimaLogger.log("SCRIPT : "+script);
		}
		
		
		ret.put("response", "yo");
		
		return ret;
	}
	
	@Override
	public Command getFunction() {
		return new multisig();
	}
}