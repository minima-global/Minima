package org.minima.system.commands.network;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.network.minima.NIOClientInfo;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class network extends Command {

	public network() {
		super("network","(action:list|reset) - Show network status or reset traffic counter");
	}
	
	@Override
	public String getFullHelp() {
		return "\nnetwork\n"
				+ "\n"
				+ "Show network status or reset traffic counter.\n"
				+ "\n"
				+ "action: (optional)\n"
				+ "    list : List the direct peers you are connected to. The default.\n"
				+ "    reset : Restart the traffic counter from 0.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "network\n"
				+ "\n"
				+ "network action:list\n"
				+ "\n"
				+ "network action:reset\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		String action = getParam("action", "list");
		
		if(action.equals("list")) {
			
			String uid = getParam("uid", "");
			
			//Get the NIO Details
			ArrayList<NIOClientInfo> clients = Main.getInstance().getNetworkManager().getNIOManager().getAllConnectionInfo();
			
			//Create a JSONArray
			JSONArray clarr = new JSONArray();
			for(NIOClientInfo info : clients) {
				if(uid.equals("")) {
					clarr.add(info.toJSON());
				}else if(uid.equals(info.getUID())){
					clarr.add(info.toJSON());
					break;
				}
			}
			
			//Add to the response
			ret.put("response", clarr);
			
		}else if(action.equals("reset")) {
			
			Main.getInstance().getNIOManager().getTrafficListener().reset();
			ret.put("response", "Traffic counter restarted..");
			
		}else if(action.equals("restart")) {
			
			//Send the message to restart the network
			Main.getInstance().PostMessage(Main.MAIN_NETRESTART);
			
			//Add to the response
			ret.put("response", "Restarting..");
		
		}else if(action.equals("loggingon")) {
		
			Main.getInstance().getNetworkManager().getNIOManager().setFullLogging(true, "");
			Main.getInstance().getNetworkManager().getP2PManager().setFullLogging(true, "");
			
			//Add to the response
			ret.put("response", "Full Network logging ON");
		
		}else if(action.equals("loggingoff")) {
			
			Main.getInstance().getNetworkManager().getNIOManager().setFullLogging(false, "");
			Main.getInstance().getNetworkManager().getP2PManager().setFullLogging(false, "");
			
			//Add to the response
			ret.put("response", "Full Network logging OFF");
		
		}else {
			throw new CommandException("Invalid action");
		}
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new network();
	}

}
