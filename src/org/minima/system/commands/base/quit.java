package org.minima.system.commands.base;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONObject;

public class quit extends Command {

	public quit() {
		super("quit","(compact:) - Shutdown Minima. Compact the Databases if you want");
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
				+ "quit\n"
				+ "\n"
				+ "quit compact:true\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"compact"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		boolean compact = getBooleanParam("compact", false);
		
		if(Main.getInstance()!=null) {
			Main.getInstance().shutdown(compact);
		}
		
		ret.put("message", "Shutdown complete");
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new quit();
	}

}
