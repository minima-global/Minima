package org.minima.system.commands.base;

import org.minima.system.commands.Command;
import org.minima.system.network.minima.NIOClient;
import org.minima.system.network.minima.NIOServer;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.MessageProcessor;

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
		
//		Main.getInstance().setTrace(on,filter);
		MessageProcessor.setTrace(on, filter);
		
		NIOClient.mTraceON = on;
		NIOServer.mTraceON = on;
		
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
