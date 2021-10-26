package org.minima.system.commands.all;

import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.network.minima.NIOManager;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class connect extends Command {

	public connect() {
		super("connect","[host:ip:port] - Connect to a network Minima instance");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		//Get the txpowid
		String fullhost = (String)getParams().get("host");
		if(fullhost == null) {
			throw new Exception("No host specified");
		}
		
		//IP and PORT
		int index 	 = fullhost.indexOf(":");
		if(index == -1) {
			throw new Exception("Must specify host:port");
		}
		
		String ip 	 = fullhost.substring(0,index).trim();
		String ports = fullhost.substring(index+1).trim();
		
		int port = 0;
		try {
			port 	 = Integer.parseInt(ports);
		}catch(NumberFormatException exc) {
			if(index == -1) {
				new Exception("Port not an integer");
			}
		}
		
		//Post a message
		Message msg = new Message(NIOManager.NIO_CONNECT);
		msg.addString("host", ip);
		msg.addInteger("port", port);
		
		Main.getInstance().getNIOManager().PostMessage(msg);
		
		ret.put("message", "Attempting to connect to "+fullhost);
	
		return ret;
	}

	@Override
	public Command getFunction() {
		return new connect();
	}

}
