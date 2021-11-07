package org.minima.system.commands.all;

import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.network.minima.NIOClientInfo;
import org.minima.system.network.p2p.P2PManager;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

import java.util.ArrayList;

public class p2pnetwork extends Command {

	public p2pnetwork() {
		super("p2pnetwork","Show p2p network status");
	}
	
	@Override
	public JSONObject runCommand() {
		JSONObject ret = getJSONReply();
		
		//Get the NIO Details
		P2PManager mgr = (P2PManager) Main.getInstance().getNetworkManager().getP2PManager();

		//Add to the response
		ret.put("response", mgr.getStatus());
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new p2pnetwork();
	}

}
