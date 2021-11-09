package org.minima.system.commands.all;

import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.network.minima.NIOManager;
import org.minima.system.network.sshtunnel.SSHManager;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class sshtunnel extends Command {

	public sshtunnel() {
		super("sshtunnel","[user:] [password:] [host:] [remoteport:] - Create an SSH Tunnel for Minima to have an external IP");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		//Get the params
		String user 	= getParam("user");
		String password = getParam("password");
		String host 	= getParam("host");
		String rport 	= getParam("remoteport");
		
		//Now send a message..
		Message startssh = new Message(SSHManager.SSHTUNNEL_START);
		startssh.addString("username", user);
		startssh.addString("password", password);
		startssh.addString("host", host);
		startssh.addInteger("remoteport", Integer.parseInt(rport));
		
		Main.getInstance().getNetworkManager().getSSHManager().PostMessage(startssh);

		ret.put("response", "SSHTunnel attempt started..");
		
//		ret.put("message", "Attempting to connect to "+fullhost);
	
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
		return new sshtunnel();
	}

}
