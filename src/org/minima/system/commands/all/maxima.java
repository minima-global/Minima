package org.minima.system.commands.all;

import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.network.maxima.Maxima;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class maxima extends Command {

	public maxima() {
		super("maxima","[function:info|new|send] (to:) (port:) (data:) - Check your Maxima identity or send a HEX data message. Port is also HEX.");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		String func = getParam("function", "info");
		
		Maxima max = Main.getInstance().getMaxima();
		if(!max.isInited()) {
			ret.put("status", false);
			ret.put("message", "Maxima still starting up..");
			return ret;
		}
		
		JSONObject details = new JSONObject();
		
		if(func.equals("info")) {
			
			//Show details
			String ident = max.getIdentity();  
			details.put("identity", ident);
			ret.put("response", details);
		
		}else if(func.equals("new")) {
			
			//Create a new Maxima Identity..
			max.createMaximaKeys();
			
			//Show details
			String ident = max.getIdentity();  
			details.put("identity", ident);
			ret.put("response", details);
			
		}else if(func.equals("send")) {
			
			if(!existsParam("to") || !existsParam("port") || !existsParam("data") ) {
				throw new Exception("MUST specify to, port and data for a send command");
			}
			
			//Send a message..
			String to 	= getParam("to");
			String port = getParam("port");
			String data = getParam("data");
			
			//Send to Maxima..
			Message sender = new Message(Maxima.MAXIMA_SENDMESSAGE);
			sender.addString("to", to);
			sender.addString("port", port);
			sender.addString("data", data);
			
			max.PostMessage(sender);
			
			ret.put("response", "Message sent..");
		}
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new maxima();
	}

}
