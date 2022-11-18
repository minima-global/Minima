package org.minima.system.commands.base;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.system.commands.Command;
import org.minima.system.network.minima.NIOClient;
import org.minima.system.network.minima.NIOServer;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.MessageProcessor;

public class trace extends Command {

	public trace() {
		super("trace","[enable:true|false] (filter:) - Show the message stacks of the internal Minima Engine with optional filter string. Only works on terminal.");
	}
	
	@Override
	public String getFullHelp() {
		return "\ntrace\n"
				+ "\n"
				+ "Show the message stacks of the internal Minima Engine with optional filter string.\n"
				+ "\n"
				+ "enable:\n"
				+ "    true or false, true to enable or false to disable.\n"
				+ "\n"
				+ "filter: (optional)\n"
				+ "    A case sensitive string to filter the messages by.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "trace enable:true\n"
				+ "\n"
				+ "trace enable:true filter:MAIN\n"
				+ "\n"
				+ "trace enable:true filter:MINER\n"
				+ "\n"
				+ "trace enable:true filter:MDS\n"
				+ "\n"
				+ "trace enable:true filter:NOTIFYMANAGER\n"
				+ "\n"
				+ "trace enable:true filter:TXPOWPROCESSOR\n"
				+ "\n"
				+ "trace enable:false\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"enable","filter"}));
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
