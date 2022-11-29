package org.minima.system.commands.network;

import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.Arrays;

import org.minima.objects.base.MiniNumber;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.network.p2p.P2PManager;
import org.minima.system.network.p2p.P2PPeersChecker;
import org.minima.system.network.p2p.messages.InetSocketAddressIO;
import org.minima.system.params.GeneralParams;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class peers extends Command {

	public peers() {
		super("peers","(action:list|addpeers) (peerslist:[]) - Prints the peers list this node has");
	}
	
	@Override
	public String getFullHelp() {
		return "\npeers\n"
				+ "\n"
				+ "Prints the peers list this node has. P2P must be enabled.\n"
				+ "\n"
				+ "Your peers are the other Minima nodes you know about.\n"
				+ "\n"
				+ "action: (optional)\n"
				+ "    list : List your peers. The default.\n"
				+ "    addpeers : Add a list of new peers. \n"
				+ "\n"
				+ "peerslist: (optional)\n"
				+ "    JSON array of new peers [ip:port,ip:port,..]\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "peers\n"
				+ "\n"
				+ "peers action:list\n"
				+ "\n"
				+ "peers action:addpeers peerslist:[\"31.125.188.214:9001\",\"94.0.239.117:9001\"]\n";
	}

	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","peerslist"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();

		//Is the P2P Enable..
		if(!GeneralParams.P2P_ENABLED) {
			throw new CommandException("P2P System not enabled");
		}
		
		//Are we adding a peer..
		String action = getParam("action","list");
		if(action.equals("list")) {
			
			P2PManager p2PManager = (P2PManager) Main.getInstance().getNetworkManager().getP2PManager();
			
			JSONObject resp = new JSONObject();
			resp.put("peers-list", InetSocketAddressIO.addressesListToJSONArray(p2PManager.getPeersCopy()));
			ret.put("response", resp);
			
			
		}else if(action.equals("addpeers")) {
			
			P2PManager p2pmanager 		= (P2PManager)Main.getInstance().getNetworkManager().getP2PManager();
			P2PPeersChecker p2pchecker 	= p2pmanager.getPeersChecker();
	        
			JSONArray peers = getJSONArrayParam("peerslist");
			for(Object peerobj : peers) {
				
				String peer = (String)peerobj; 
				
				//Check is a valid host:port
				Message checker = connect.createConnectMessage(peer);
				if(checker == null) {
					throw new CommandException("Invalid peer : "+peer);
				}
				
				//Create an address
				InetSocketAddress addr = new InetSocketAddress(checker.getString("host"), checker.getInteger("port"));
				
				//Otherwise add
				Message msg = new Message(P2PPeersChecker.PEERS_CHECKPEERS).addObject("address", addr);
				msg.addBoolean("force", true);
				
				p2pchecker.PostMessage(msg);
			}
			
		}else {
			throw new CommandException("Invalid action : "+action);
		}
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new peers();
	}

}
