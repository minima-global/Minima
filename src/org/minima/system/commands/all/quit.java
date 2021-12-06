package org.minima.system.commands.all;

import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONObject;

public class quit extends Command {

	public quit() {
		super("quit","Shutdown Minima");
	}
	
	@Override
	public JSONObject runCommand() {
		JSONObject ret = getJSONReply();
		
		Main.getInstance().shutdown();
		
		ret.put("message", "Shutdown complete");
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new quit();
	}

}
