package org.minima.system.commands.network;

import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.network.p2p.P2PManager;
import org.minima.utils.json.JSONObject;

public class p2pstate extends Command {

	public p2pstate() {
		super("p2pstate","prints full details of the internal p2p state");
	}

	@Override
	public String getFullHelp() {
		return "\np2pstate\n"
				+ "\n"
				+ "Prints full details of the internal p2p state.\n"
				+ "\n"
				+ "Includes details of your in and out connections and total peers.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "p2pstate\n";
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();

		P2PManager p2PManager = (P2PManager) Main.getInstance().getNetworkManager().getP2PManager();
		ret.put("p2p-state", p2PManager.getStatus(true));
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new p2pstate();
	}

}
