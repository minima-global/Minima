package org.minima.system.commands.network;

import java.io.File;
import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.StringTokenizer;

import org.minima.database.MinimaDB;
import org.minima.objects.base.MiniNumber;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.network.p2p.P2PManager;
import org.minima.system.network.p2p.P2PPeersChecker;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MiniFile;
import org.minima.utils.MinimaLogger;
import org.minima.utils.RPCClient;
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
		return new ArrayList<>(Arrays.asList(new String[]{"action","peerslist","max","file","url"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();

		//Is the P2P Enable..
		if(!GeneralParams.P2P_ENABLED || MinimaDB.getDB().getUserDB().isSlaveNode()) {
			JSONObject resp = new JSONObject();
			resp.put("peers-list", "");
			resp.put("havepeers",true);
			resp.put("p2penabled",GeneralParams.P2P_ENABLED);
			resp.put("message","P2P System NOT enabled");
			ret.put("response", resp);
			return ret;
		}
		
		//Are we adding a peer..
		String action = getParam("action","list");
		if(action.equals("list")) {
			
			//How many peers to show
			int maxpeers = getNumberParam("max", MiniNumber.THOUSAND).getAsInt();
			String peerslist = getPeersList(maxpeers);
			int numberpeers  = peerslist.split(",").length; 
			if(peerslist.trim().equals("")) {
				numberpeers = 0;
			}
			
			P2PManager p2PManager = (P2PManager) Main.getInstance().getNetworkManager().getP2PManager();
			
			JSONObject resp = new JSONObject();
			resp.put("peerslist", peerslist);
			resp.put("size", numberpeers);
			resp.put("havepeers",p2PManager.haveAnyPeers());
			resp.put("p2penabled",GeneralParams.P2P_ENABLED);
			ret.put("response", resp);
		
		}else if(action.equals("forcecheck")) {
			
			P2PManager p2PManager = (P2PManager) Main.getInstance().getNetworkManager().getP2PManager();
			P2PPeersChecker checker = p2PManager.getPeersChecker();
			checker.PostMessage(P2PPeersChecker.PEERS_FORCEFULLCHECK);
			
			int maxpeers 		= getNumberParam("max", MiniNumber.THOUSAND).getAsInt();
			String peerslist 	= getPeersList(maxpeers);
			int numberpeers  	= peerslist.split(",").length;
			
			JSONObject resp = new JSONObject();
			resp.put("message", "Peers check started.. will start ASAP");
			resp.put("logs", GeneralParams.PEERSCHECKER_lOG);
			resp.put("size", numberpeers);
			ret.put("response", resp);
			
		}else if(action.equals("addpeers")) {
			
			P2PManager p2pmanager 		= (P2PManager)Main.getInstance().getNetworkManager().getP2PManager();
			P2PPeersChecker p2pchecker 	= p2pmanager.getPeersChecker();
	        
			//Is it a single IP
			String peerstr = getParam("peerslist"); 
			
			//Is it an URL ? Download the list
			if(peerstr.startsWith("http")) {
				
				//Download the list
				String urllist = RPCClient.sendGET(peerstr);
				if(urllist.equals("")) {
					throw new CommandException("No peers found @ "+peerstr);
				}
				
				MinimaLogger.log("Peers downloaded "+urllist);
				
				peerstr = urllist;
			}
			
			//Break up
			ArrayList<String> validpeers   = new ArrayList<>();
			ArrayList<String> invalidpeers = new ArrayList<>();
			StringTokenizer strtok = new StringTokenizer(peerstr,",");
			while(strtok.hasMoreTokens()) {
				String peer = strtok.nextToken();
				
				//Get the IP..
				Message checker = connect.createConnectMessage(peer);
				if(checker != null) {
					
					//Create an address
					InetSocketAddress addr = new InetSocketAddress(checker.getString("host"), checker.getInteger("port"));
					
					//Now send to the peers checker..
					Message msg = new Message(P2PPeersChecker.PEERS_CHECKPEERS).addObject("address", addr);
					msg.addBoolean("force", true);
					
					p2pchecker.PostMessage(msg);
					validpeers.add(peer);
				}else {
					invalidpeers.add(peer);
				}
			}
			
			JSONObject resp = new JSONObject();
			resp.put("valid",validpeers.toString());
			resp.put("invalid",invalidpeers.toString());
			resp.put("message","Valid Peers added to checking queue..");
			ret.put("response", resp);
			
		}else if(action.equals("publish")) {
			
			String file=getParam("file","peerslist.txt");
			
			String peerslist = getPeersList(20);
		
			//And now publish to the file..
			File ff = MiniFile.createBaseFile(file);
			
			MiniFile.writeDataToFile(ff, peerslist.getBytes());
		
			JSONObject resp = new JSONObject();
			resp.put("peers",peerslist);
			resp.put("file",ff.getAbsolutePath());
			ret.put("response", resp);
			
		}else if(action.equals("fetch")) {
			
			String peerstr = "";
			
			String url = "";
			if(existsParam("file")) {
				url = getParam("file");
				File ff = MiniFile.createBaseFile(url);
				byte[] pdata = MiniFile.readCompleteFile(ff);
				peerstr = new String(pdata);
			}else{
				url = getParam("url");
				peerstr = RPCClient.sendGET(url);
			}
			
			
			if(peerstr.equals("")) {
				throw new CommandException("No peers found in location "+url);
			}
			
			P2PManager p2pmanager 		= (P2PManager)Main.getInstance().getNetworkManager().getP2PManager();
			P2PPeersChecker p2pchecker 	= p2pmanager.getPeersChecker();
	        
			//And now add those peers
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
			resp.put("peers",peerstr);
			resp.put("location",url);
			resp.put("message","Peers added to checking queue..");
			ret.put("response", resp);
			
		}else {
			throw new CommandException("Invalid action : "+action);
		}
		
		return ret;
	}

	public static String getPeersList(int zMaxPeers) {
		P2PManager p2PManager = (P2PManager) Main.getInstance().getNetworkManager().getP2PManager();
		
		//Get the peers list
		ArrayList<InetSocketAddress> peers = p2PManager.getPeersCopy();
		
		//Shuffle it..
		Collections.shuffle(peers);
		
		//Now add to the list..
		String peerslist = "";
		int counter=0;
		for(InetSocketAddress peer : peers) {
			
			//Check limit
			if(counter>zMaxPeers) {
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
		
		return peerslist;
	}
	
	@Override
	public Command getFunction() {
		return new peers();
	}

}
