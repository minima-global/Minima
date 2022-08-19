package org.minima.system.commands.network;

import java.net.InetSocketAddress;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.minima.objects.Greeting;
import org.minima.system.commands.Command;
import org.minima.system.network.minima.NIOManager;
import org.minima.system.network.p2p.messages.InetSocketAddressIO;
import org.minima.system.network.p2p.params.P2PParams;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class nodecount extends Command {

	public nodecount() {
		super("nodecount","");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		Set<InetSocketAddress> allPeers = new HashSet<>();
		Set<InetSocketAddress> pingedPeers = new HashSet<>();

		for (InetSocketAddress addr: P2PParams.DEFAULT_NODE_LIST)
		{
			Greeting greet = NIOManager.sendPingMessage(addr.getHostString(), addr.getPort(), true);
			if (greet != null) {
				JSONArray peersArrayList = (JSONArray) greet.getExtraData().get("peers-list");
				if (peersArrayList != null) {
//                    MinimaLogger.log(peersArrayList.toString());
					List<InetSocketAddress> newPeers = InetSocketAddressIO.addressesJSONArrayToList(peersArrayList);
					allPeers.addAll(newPeers);
				}
			}
			pingedPeers.add(addr);
		}
		boolean keepGoing = true;
		Set<InetSocketAddress> peersToCheck = new HashSet<>(allPeers);
		peersToCheck.removeAll(pingedPeers);

		while (keepGoing){
			for (InetSocketAddress addr: peersToCheck){
				Greeting greet = NIOManager.sendPingMessage(addr.getHostString(), addr.getPort(), true);
				if (greet != null) {
					JSONArray peersArrayList = (JSONArray) greet.getExtraData().get("peers-list");
					if (peersArrayList != null) {
//                    MinimaLogger.log(peersArrayList.toString());
						List<InetSocketAddress> newPeers = InetSocketAddressIO.addressesJSONArrayToList(peersArrayList);
						allPeers.addAll(newPeers);
					}
				}
				pingedPeers.add(addr);
				MinimaLogger.log("Total unique peers: " + allPeers.size() + " Peers Checked: " + pingedPeers.size());
			}
			peersToCheck = new HashSet<>(allPeers);
			peersToCheck.removeAll(pingedPeers);
			if (peersToCheck.size() == 0){
				keepGoing = false;
			}
		}

		return ret;
	}

	@Override
	public Command getFunction() {
		return new nodecount();
	}

}
