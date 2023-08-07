package org.minima.system.commands.network;

import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.StringTokenizer;

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
		super("peers","(action:list|addpeers) (peerslist:) - Prints the peers list this node has");
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
				+ "    CSV list of new peers [ip:port,ip:port,..]\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "peers\n"
				+ "\n"
				+ "peers action:list\n"
				+ "\n"
				+ "peers action:addpeers peerslist:31.125.188.214:9001,94.0.239.117:9001\n";
	}

	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","peerslist","max"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();

		//Is the P2P Enable..
		if(!GeneralParams.P2P_ENABLED) {
			JSONObject resp = new JSONObject();
			resp.put("peers-list", "");
			resp.put("havepeers",false);
			resp.put("p2penabled",false);
			resp.put("message","P2P System NOT enabled");
			ret.put("response", resp);
			return ret;
		}
		
		//Are we adding a peer..
		String action = getParam("action","list");
		if(action.equals("list")) {
			
			P2PManager p2PManager = (P2PManager) Main.getInstance().getNetworkManager().getP2PManager();
			
			//How many peers to show
			int maxpeers = getNumberParam("max", MiniNumber.THOUSAND).getAsInt();
			
			//Get the peers list
			ArrayList<InetSocketAddress> peers = p2PManager.getPeersCopy();
			
			//Shuffle it..
			Collections.shuffle(peers);
			
			//Now add to the list..
			String peerslist = "";
			int counter=0;
			for(InetSocketAddress peer : peers) {
				
				//Check limit
				if(counter>maxpeers) {
					break;
				}
				
				//Add it..
				peerslist += peer.getAddress().getHostAddress() + ":" + peer.getPort()+",";
			
				counter++;
			}
			
			//Remove the final ,
			if(peerslist.endsWith(",")) {
				peerslist = peerslist.substring(0, peerslist.length()-1);
			}
			
			JSONObject resp = new JSONObject();
			resp.put("peerslist", peerslist);
			resp.put("havepeers",p2PManager.haveAnyPeers());
			resp.put("p2penabled",true);
			ret.put("response", resp);
			
		}else if(action.equals("addpeers")) {
			
			P2PManager p2pmanager 		= (P2PManager)Main.getInstance().getNetworkManager().getP2PManager();
			P2PPeersChecker p2pchecker 	= p2pmanager.getPeersChecker();
	        
			//Is it a single IP
			String peerstr = getParam("peerslist"); 
			
			//Break up 
			StringTokenizer strtok = new StringTokenizer(peerstr,",");
			while(strtok.hasMoreTokens()) {
				String peer = strtok.nextToken();
				
				//Get the IP..
				Message checker = connect.createConnectMessage(peer);
				if(checker == null) {
					throw new CommandException("Invalid peer : "+peer);
				}
				
				//Create an address
				InetSocketAddress addr = new InetSocketAddress(checker.getString("host"), checker.getInteger("port"));
				
				//Now send to the peers checker..
				Message msg = new Message(P2PPeersChecker.PEERS_CHECKPEERS).addObject("address", addr);
				msg.addBoolean("force", true);
				
				p2pchecker.PostMessage(msg);
			}
			
			JSONObject resp = new JSONObject();
			resp.put("message","Peers added to checking queue..");
			ret.put("response", resp);
			
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
