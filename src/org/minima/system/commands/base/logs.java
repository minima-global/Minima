package org.minima.system.commands.base;

import org.minima.system.commands.Command;
import org.minima.system.params.GeneralParams;
import org.minima.utils.json.JSONObject;

public class logs extends Command {

	public logs() {
		super("logs","(scripts:true|false) - Enable full logs for various parts of Minima");
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();

		String scripts = getParam("scripts", "false");
		
		//For now - stop accepting 
		if(scripts.equals("true")) {
			//Accept txpow messages
			GeneralParams.SCRIPTLOGS = true;
		}else {
			//Don't accept txpow messages - pulse does that
			GeneralParams.SCRIPTLOGS= false;
		}
		
		JSONObject resp = new JSONObject();
		resp.put("scripts", GeneralParams.SCRIPTLOGS);
		
		//Add balance..
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new logs();
	}

}
