package org.minima.system.commands.network;

import java.io.File;
import java.io.PrintWriter;
import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.minima.objects.Greeting;
import org.minima.system.commands.Command;
import org.minima.system.network.minima.NIOManager;
import org.minima.system.network.p2p.messages.InetSocketAddressIO;
import org.minima.system.network.p2p.params.P2PParams;
import org.minima.utils.MiniFile;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class nodecount extends Command {

	public nodecount() {
		super("nodecount","(file:) (complete:false|true) - Enumerate the network to count all participating nodes");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"file","complete"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		boolean saveCSV = true;
		String file = getParam("file","");
		PrintWriter writer = null;
		JSONObject details = new JSONObject();
		if(file.equals("")) {
			saveCSV = false;
		} else {
			File csvFile = MiniFile.createBaseFile(file);
			writer = new PrintWriter(csvFile);
		}

		boolean complete = getBooleanParam("complete", false);

		StringBuilder sb = new StringBuilder();

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
		double totalClients = 0;



		if (saveCSV) {
			sb.append("num_checked");
			sb.append(',');
			sb.append("total_peers");
			sb.append(',');
			sb.append("num_clients");
			sb.append('\n');
			writer.write(sb.toString());
		}
		float scan_percent = (float) pingedPeers.size() / allPeers.size() / 0.06f * 100;
		while (keepGoing){
			for (InetSocketAddress addr: peersToCheck){
				Greeting greet = NIOManager.sendPingMessage(addr.getHostString(), addr.getPort(), true);
				if (greet != null) {
					JSONArray peersArrayList = (JSONArray) greet.getExtraData().get("peers-list");
					String clients  = greet.getExtraData().getOrDefault("clients", 0.0f).toString();
					totalClients += Double.parseDouble(clients);
					if (peersArrayList != null) {
//                    MinimaLogger.log(peersArrayList.toString());
						List<InetSocketAddress> newPeers = InetSocketAddressIO.addressesJSONArrayToList(peersArrayList);
						allPeers.addAll(newPeers);
					}
				}
				pingedPeers.add(addr);
				scan_percent = (float) pingedPeers.size() / allPeers.size() / 0.06f * 100;
				if (saveCSV) {
					sb = new StringBuilder();
					sb.append(pingedPeers.size());
					sb.append(',');
					sb.append(allPeers.size());
					sb.append(',');
					sb.append(totalClients);
					sb.append('\n');
					writer.write(sb.toString());
					writer.flush();
				}
				if (scan_percent >= 100 && !complete){
					keepGoing = false;
					break;
				}
				if (complete) {
					scan_percent = (float) pingedPeers.size() / allPeers.size() * 100;
				}
				MinimaLogger.log("Peers Checked: " + pingedPeers.size() + " Scanning... " + scan_percent + "%");
			}
			peersToCheck = new HashSet<>(allPeers);
			peersToCheck.removeAll(pingedPeers);
			if (peersToCheck.size() == 0 || scan_percent >= 100){
				keepGoing = false;
				float clients_per_server = (float) totalClients / pingedPeers.size();
				// After 6% of the network has been scanned over 95% of p2p nodes have been discovered
				// multiplying the total peers to give a projected total node count.

					float total_servers = allPeers.size() * 1.05f;
					float clients = clients_per_server * total_servers;
					float total_nodes = total_servers + clients;

					if (complete){
						total_servers = allPeers.size();
						clients = (float) totalClients;
						total_nodes = total_servers + clients;
					}

				MinimaLogger.log("Total participating nodes: " + (int) total_nodes + " of which " + (int) total_servers + " are p2p servers and " + (int) clients + " are client nodes." );
				details.put("total_nodes", total_nodes);
				details.put("server_nodes", total_servers);
				details.put("client_nodes", clients);

			}
		}

		ret.put("response", details);
		return ret;
	}

	@Override
	public Command getFunction() {
		return new nodecount();
	}

}
