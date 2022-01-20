package org.minima.system.commands.base;

import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONObject;

public class trace extends Command {

	public trace() {
		super("trace","[enable:true|false] (filter:) - Show the message stacks of the internal Minima Engine with optional filter string");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		String act = (String)getParams().get("enable");
		if(act == null) {
			throw new Exception("Must specify enable");
		}
	
		boolean on = false;
		if(act.equals("true")) {
			on = true;
		}
	
		String filter = getParam("filter", "");
		
		Main.getInstance().setTrace(on,filter);
		
		JSONObject tr = new JSONObject();
		tr.put("enabled", on);
		tr.put("filter", filter);
		
		ret.put("response", tr);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new trace();
	}

}
