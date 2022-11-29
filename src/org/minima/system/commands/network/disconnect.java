package org.minima.system.commands.network;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.network.minima.NIOManager;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class disconnect extends Command {

	public disconnect() {
		super("disconnect","[uid:uid] - Disconnect from a connected or connecting host");
	}
	
	@Override
	public String getFullHelp() {
		return "\ndisconnect\n"
				+ "\n"
				+ "Disconnect from a connected or connecting host.\n"
				+ "\n"
				+ "Optionally disconnect from all hosts.\n"
				+ "\n"
				+ "uid:\n"
				+ "    Use 'all' to disconnect from all hosts or enter the uid of the host to disconnect from.\n"
				+ "    uid can be found from the 'network' command.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "disconnect uid:CVNPMLPOCQ0HQ\n"
				+ "\n"
				+ "disconnect uid:all\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"uid"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		//Get the txpowid
		String uid = (String) getParams().get("uid");
		
		if(uid == null) {
			throw new Exception("No uid specified");
		}
		
		if(uid.equals("all")) {
			Main.getInstance().getNIOManager().PostMessage(new Message(NIOManager.NIO_DISCONNECTALL));
			
		}else {
			Main.getInstance().getNIOManager().disconnect(uid);
		}
		
		ret.put("status", true);
		ret.put("message", "Attempting to disconnect from "+uid);
	
		return ret;
	}

	@Override
	public Command getFunction() {
		return new disconnect();
	}

}
