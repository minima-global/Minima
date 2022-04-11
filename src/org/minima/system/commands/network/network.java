package org.minima.system.commands.network;

import java.util.ArrayList;

import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.network.minima.NIOClientInfo;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class network extends Command {

	public network() {
		super("network","(action:list|restart) - Show network status or restart");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		String action = getParam("action", "list");
		
		if(action.equals("list")) {
			//Get the NIO Details
			ArrayList<NIOClientInfo> clients = Main.getInstance().getNetworkManager().getNIOManager().getAllConnectionInfo();
			
			//Create a JSONArray
			JSONArray clarr = new JSONArray();
			for(NIOClientInfo info : clients) {
				clarr.add(info.toJSON());
			}
			
			//Add to the response
			ret.put("response", clarr);
			
		}else if(action.equals("restart")) {
			
			//Send tyhe message to restart the network
			Main.getInstance().PostMessage(Main.MAIN_NETRESTART);
			
			//Add to the response
			ret.put("response", "Restarting..");
		
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
