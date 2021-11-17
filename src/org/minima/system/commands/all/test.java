package org.minima.system.commands.all;

import org.minima.system.commands.Command;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;

public class test extends Command {

	public test() {
		super("test","test Funxtion");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		//Get a JSON parameter
		JSONObject json = getJSONParam("json");
		
		MinimaLogger.log("JSON Received : "+json.toString());
		
		ret.put("response", "");
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new test();
	}

}
