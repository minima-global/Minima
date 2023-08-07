package org.minima.system.commands.mds;

import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONObject;

public class checkrestore extends Command {

	public checkrestore() {
		super("checkrestore","Check if the system is restoring");
	}
	
	@Override
	public String getFullHelp() {
		return  "checkrestore\n"
				+ "\n"
				+ "Check if Minima is restoring\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "checkrestore\n";
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		//Who called it
		String minidappid = getMiniDAPPID();
		
		JSONObject resp = new JSONObject();
		resp.put("restoring", Main.getInstance().isRestoring());
		resp.put("shuttingdown", Main.getInstance().isShuttingDown());
		resp.put("complete", Main.getInstance().isShutdownComplete());
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new checkrestore();
	}

}
