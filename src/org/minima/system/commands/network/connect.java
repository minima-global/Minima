package org.minima.system.commands.network;

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
		
		//Get the host:port
		String fullhost = (String)getParams().get("host");
		if(fullhost == null) {
			throw new Exception("No host specified");
		}
		
		//Create the Message
		Message connect = createConnectMessage(fullhost);
		if(connect == null) {
			throw new Exception("Must specify host:port");
		}
		
		Main.getInstance().getNIOManager().PostMessage(connect);
		
		ret.put("message", "Attempting to connect to "+fullhost);
	
		return ret;
	}

	public static Message createConnectMessage(String zFullHost) {
		//IP and PORT
		int index = zFullHost.indexOf(":");
		if(index == -1) {
			return null;
		}
		
		String ip 	 = zFullHost.substring(0,index).trim();
		String ports = zFullHost.substring(index+1).trim();
		
		int port = 0;
		try {
			port = Integer.parseInt(ports);
		}catch(NumberFormatException exc) {
			return null;
		}
		
		//Post a message
		Message msg = new Message(NIOManager.NIO_CONNECT);
		msg.addString("host", ip);
		msg.addInteger("port", port);
		
		return msg;
	}
	
	@Override
	public Command getFunction() {
		return new connect();
	}

}
