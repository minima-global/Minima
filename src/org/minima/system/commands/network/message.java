package org.minima.system.commands.network;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.network.minima.NIOMessage;
import org.minima.utils.json.JSONObject;

public class message extends Command {

	public message() {
		super("message","[data:message] (uid:uid) - Send a message over the network to one of your direct peers");
	}
	
	@Override
	public String getFullHelp() {
		return "\nmessage\n"
				+ "\n"
				+ "Send a message to one or all of your direct peers.\n"
				+ "\n"
				+ "data:\n"
				+ "    The message as a string.\n"
				+ "\n"
				+ "uid: (optional)\n"
				+ "    Leave blank to send a message to all peers or enter the uid of the peer to send the message to.\n"
				+ "    uid can be found from the 'network' command.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "message data:\"hello\" uid:CVNPMLPOCQ0HQ\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"uid","data"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		//get the data
		String data = (String) getParams().get("data");
		if(data == null) {
			throw new Exception("No data specified");
		}
		
		//Is there a UID
		String uid = (String) getParams().get("uid");
		if(uid == null) {
			uid = "";
			ret.put("message", "Message sent to all");
		}else {
			ret.put("message", "Message sent to "+uid);
		}
		
		//Create a message..
		MiniString msg 	= new MiniString(data);
		
		//Send it..
		Main.getInstance().getNIOManager().sendNetworkMessage(uid, NIOMessage.MSG_GENMESSAGE, msg);
			
		return ret;
	}

	@Override
	public Command getFunction() {
		return new message();
	}

}
