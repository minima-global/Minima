package org.minima.system.commands.all;

import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONObject;

public class network extends Command {

	public network() {
		super("network","Show network status");
	}
	
	@Override
	public JSONObject runCommand() {
		JSONObject ret = getJSONReply();
		
		//Get the NIO Details
		JSONObject nio = new JSONObject();
		nio.put("nio", Main.getInstance().getNetworkManager().getNIOManager().getAllConnections());
		ret.put("response", nio);
		
		//Get the P2P details
		//..
		
		//Get the RPC details..
		//..
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new network();
	}

}
