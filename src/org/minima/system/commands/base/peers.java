package org.minima.system.commands.base;

import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.network.p2p.P2PManager;
import org.minima.system.network.p2p.messages.InetSocketAddressIO;
import org.minima.system.params.GeneralParams;
import org.minima.utils.json.JSONObject;

public class peers extends Command {

	public peers() {
		super("peers","prints the peers list this node has");
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();

		//Is the P2P Enable..
		if(!GeneralParams.P2P_ENABLED) {
			throw new CommandException("P2P System not enabled");
		}
		
		
		P2PManager p2PManager = (P2PManager) Main.getInstance().getNetworkManager().getP2PManager();
		
		JSONObject resp = new JSONObject();
		resp.put("peers-list", InetSocketAddressIO.addressesListToJSONArray(p2PManager.getPeersCopy()));
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new peers();
	}

}
