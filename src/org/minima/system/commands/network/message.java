package org.minima.system.commands.network;

import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.network.minima.NIOMessage;
import org.minima.utils.json.JSONObject;

public class message extends Command {

	public message() {
		super("message","(uid:uid) [data:message] - Send a message over the network to one of your direct peers");
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
