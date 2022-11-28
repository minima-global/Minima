package org.minima.system.commands.base;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.system.commands.Command;
import org.minima.system.params.GeneralParams;
import org.minima.utils.json.JSONObject;

public class logs extends Command {

	public logs() {
		super("logs","(scripts:true|false) (mining:true|false) - Enable full logs for various parts of Minima");
	}
	
	@Override
	public String getFullHelp() {
		return "\nlogs\n"
				+ "\n"
				+ "Enable detailed logs for script errors or mining activity.\n"
				+ "\n"
				+ "scripts: (optional)\n"
				+ "    true or false, true turns on detailed logs for script errors.\n"
				+ "\n"
				+ "mining: (optional)\n"
				+ "    true or false, true turns on detailed logs for mining start/end activity.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "logs scripts:true\n"
				+ "\n"
				+ "logs scripts:false mining:true\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"scripts","mining"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();

		//Are we logging script errors
		if(existsParam("scripts")) {
			String scripts = getParam("scripts", "false");
			if(scripts.equals("true")) {
				//Accept txpow messages
				GeneralParams.SCRIPTLOGS = true;
			}else {
				//Don't accept txpow messages - pulse does that
				GeneralParams.SCRIPTLOGS= false;
			}
		}
		
		//Are we logging all mining
		if(existsParam("mining")) {
			String mining = getParam("mining", "false");
			if(mining.equals("true")) {
				//Accept txpow messages
				GeneralParams.MINING_LOGS = true;
			}else {
				//Don't accept txpow messages - pulse does that
				GeneralParams.MINING_LOGS= false;
			}
		}
		
		JSONObject resp = new JSONObject();
		resp.put("scripts", GeneralParams.SCRIPTLOGS);
		resp.put("mining", GeneralParams.MINING_LOGS);
		
		//Add balance..
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new logs();
	}

}
