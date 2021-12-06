package org.minima.system.commands.all;

import org.minima.system.commands.Command;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class test extends Command {

	public test() {
		super("test","test Funxtion");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		//Get a JSON parameter
//		JSONObject json = getJSONParam("json");
//		MinimaLogger.log("JSON Received : "+json.toString());
		
		if(isParamJSONObject("jj")) {
			JSONObject jsonp = getJSONObjectParam("jj");
			
			MinimaLogger.log("JSON : "+jsonp.toJSONString());
			
		}else {
			MinimaLogger.log("STRING : "+getParam("jj"));
		}
		
		JSONArray jarr = getJSONArrayParam("arr");
		
		
		ret.put("response", jarr);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new test();
	}

}
