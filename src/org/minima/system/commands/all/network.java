package org.minima.system.commands.all;

import java.util.ArrayList;

import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.network.minima.NIOClientInfo;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class network extends Command {

	public network() {
		super("network","Show network status");
	}
	
	@Override
	public JSONObject runCommand() {
		JSONObject ret = getJSONReply();
		
		//Get the NIO Details
		ArrayList<NIOClientInfo> clients = Main.getInstance().getNetworkManager().getNIOManager().getAllConnectionInfo();
		
		//Create a JSONArray
		JSONArray clarr = new JSONArray();
		for(NIOClientInfo info : clients) {
			clarr.add(info.toJSON());
		}
		
		//Add to the response
		ret.put("response", clarr);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new network();
	}

}
