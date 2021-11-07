package org.minima.system.commands.all;

import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONObject;

public class trace extends Command {

	public trace() {
		super("trace","[activate:on|off] - Show the message stacks of the internal Minima Engine");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		String act = (String)getParams().get("activate");
		if(act == null) {
			throw new Exception("Must specify activate on or off");
		}
	
		boolean on = true;
		if(act.equals("off")) {
			on = false;
		}
		
		Main.getInstance().setTrace(on);
		
		ret.put("message", "Trace "+on);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new trace();
	}

}
