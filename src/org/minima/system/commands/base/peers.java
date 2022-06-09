package org.minima.system.commands.base;

import java.util.ArrayList;

import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.network.p2p.P2PManager;
import org.minima.system.network.p2p.messages.InetSocketAddressIO;
import org.minima.utils.json.JSONObject;

public class peers extends Command {

	public peers() {
		super("peers","prints the peers list this node has");
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();

		P2PManager p2PManager = (P2PManager) Main.getInstance().getNetworkManager().getP2PManager();
		ret.put("peers-list", InetSocketAddressIO.addressesListToJSONArray(new ArrayList<>(p2PManager.getPeers())));
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new peers();
	}

}
