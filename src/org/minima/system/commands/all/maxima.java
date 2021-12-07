package org.minima.system.commands.all;

import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.network.maxima.Maxima;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class maxima extends Command {

	public maxima() {
		super("maxima","[function:info|send] (to:) (application:) (message:) - Check your Maxima identity or send a message");
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
			
			if(!existsParam("to") || !existsParam("application") || !existsParam("message") ) {
				throw new Exception("MUST specify to, application and message for a send command");
			}
			
			//Send a message..
			String fullto 	= getParam("to");
			int indexp 		= fullto.indexOf("@");
			int index 		= fullto.indexOf(":");
			
			//Get the Public Key
			String publickey = fullto.substring(0,indexp);
			
			//get the host and port..
			String tohost 	= fullto.substring(indexp+1,index);
			int toport		= Integer.parseInt(fullto.substring(index+1));	
			
			//Which application
			String application 	= getParam("application");
			String message 		= getParam("message");
			
			//Send to Maxima..
			Message sender = new Message(Maxima.MAXIMA_SENDMESSAGE);
			sender.addString("publickey", publickey);
			sender.addString("tohost", tohost);
			sender.addInteger("toport", toport);
			sender.addString("application", application);
			sender.addString("message", message);
			
			max.PostMessage(sender);
			
			ret.put("response", sender.toString());
		}
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new maxima();
	}

}
