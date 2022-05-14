package org.minima.system.commands.network;

import org.minima.objects.Greeting;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.json.JSONObject;

public class ping extends Command {

	public ping() {
		super("ping","[host:] - Ping a host and get back Minima Node info");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		String host = getParam("host");
		
		int index = host.indexOf(":");
		if(index == -1) {
			return null;
		}
		
		String ip 	 = host.substring(0,index).trim();
		String ports = host.substring(index+1).trim();
		
		int port = 0;
		try {
			port = Integer.parseInt(ports);
		}catch(NumberFormatException exc) {
			throw new CommandException("Invalid port : "+ports);
		}
		
		//Call the ping function..
		Greeting greet = Main.getInstance().getNIOManager().sendPingMessage(ip, port);
		JSONObject resp = new JSONObject();
		resp.put("host", ip);
		resp.put("port", port);
		
		if(greet == null) {
			resp.put("valid", false);
		}else {
			resp.put("valid", true);
			resp.put("version", greet.getVersion());
			resp.put("extradata", greet.getExtraData());
		}
			
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new ping();
	}

}
