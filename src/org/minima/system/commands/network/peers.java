package org.minima.system.commands.network;

import java.net.InetSocketAddress;

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
		super("peers","prints the peers list this node has");
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
