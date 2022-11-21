package org.minima.system.commands.base;

import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONObject;

public class quit extends Command {

	public quit() {
		super("quit","Shutdown Minima");
	}
	
	@Override
	public String getFullHelp() {
		return "\nquit\n"
				+ "\n"
				+ "Shutdown Minima safely.\n"
				+ "\n"
				+ "Ensure you have a backup before shutting down.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "quit\n";
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
