package org.minima.system.commands.mds;

import org.minima.system.commands.Command;
import org.minima.utils.json.JSONObject;

public class mds extends Command {

	public mds() {
		super("mds","action:list|install|uninstall|reload - MiniDAPP System management");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();

		
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new mds();
	}

}
