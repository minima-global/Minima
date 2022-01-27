package org.minima.system.commands.network;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.network.maxima.Maxima;
import org.minima.system.network.maxima.MaximaMessage;
import org.minima.utils.Crypto;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class maxima extends Command {

	public maxima() {
//		super("maxima","[action:info|send|addclient|removeclient|sethost] (to:) (application:) (data:) (logs:true|false) - Check your Maxima details, send a message / data, enable logs");
		super("maxima","[action:info|send|sethost] (to:) (application:) (data:) (logs:true|false) - Check your Maxima details, send a message / data, enable logs");
	}
	
	@Override
	public String getFullHelp() {
		return "Maxima is an information transport layer running ontop of Minima";
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		String func = getParam("action", "info");
		
		Maxima max = Main.getInstance().getMaxima();
		if(!max.isInited()) {
			ret.put("status", false);
			ret.put("message", "Maxima still starting up..");
			return ret;
		}
		
		//Enable Logs..
		if(existsParam("logs")) {
			if(getParam("logs").equals("true")) {
				max.mMaximaLogs = true;
			}else {
				max.mMaximaLogs = false;
			}
		}
		
		JSONObject details = new JSONObject();
		
		if(func.equals("info")) {
			
			//Show details
			details.put("identity", max.getFullIdentity());
			details.put("external", max.isHostSet());
			details.put("host", max.getMaximaHost());
			
			//Get the current client list
			
			
			
			details.put("clients", max.getMaximaClients());
			details.put("logs", max.mMaximaLogs);
			ret.put("response", details);
		
		}else if(func.equals("new")) {
			
			//Create a new Maxima Identity..
			max.createMaximaKeys();
			
			//Show details
			String ident = max.getFullIdentity();  
			details.put("identity", ident);
			ret.put("response", details);
		
		}else if(func.equals("addclient")) {
			
			if(!existsParam("data")) {
				throw new Exception("MUST specify client in the 'data' param");
			}
			String client = getParam("data");
			max.addValidMaximaClient(client);
			
			details.put("clients", max.getMaximaClients());
			ret.put("response", details);
		
		}else if(func.equals("removeclient")) {
			
			if(!existsParam("data")) {
				throw new Exception("MUST specify client in the 'data' param");
			}
			String client = getParam("data");
			max.removeValidMaximaClient(client);
			
			details.put("clients", max.getMaximaClients());
			ret.put("response", details);
		
		}else if(func.equals("sethost")) {
			
			if(!existsParam("data")) {
				throw new Exception("MUST specify host in the 'data' param");
			}
			String host = getParam("data");
			max.setMaximaHost(host);
			
			details.put("host", max.getMaximaHost());
			ret.put("response", details);
		
		}else if(func.equals("send")) {
			
			if(!existsParam("to") || !existsParam("application") || !existsParam("data") ) {
				throw new Exception("MUST specify to, application and data for a send command");
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
			
			String data = "";
			if(isParamJSONObject("data")) {
				data = getJSONObjectParam("data").toString();
			}else {
				data = getParam("data");
			} 
			
			MiniData mdata = null;
			if(data.startsWith("0x") || data.startsWith("Mx")) {
				mdata = new MiniData(data);
			}else {
				mdata = new MiniData(new MiniString(data).getData());
			}
			
			//Get the complete details..
			MaximaMessage maxmessage = max.createMaximaMessage(fullto, application, mdata);
			
			//Get the MinData version
			MiniData maxdata = MiniData.getMiniDataVersion(maxmessage);
			
			//Hash it..
			MiniData hash 	= Crypto.getInstance().hashObject(maxdata);
			
			//Convert to JSON
			JSONObject json = maxmessage.toJSON();
			json.put("msgid", hash.to0xString());
			
			//Send to Maxima..
			Message sender = new Message(Maxima.MAXIMA_SENDMESSAGE);
			sender.addObject("maxima", maxmessage);
			sender.addString("publickey", publickey);
			sender.addString("tohost", tohost);
			sender.addInteger("toport", toport);
			
			//Post It!
			max.PostMessage(sender);
			
			ret.put("response", json);
		}
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new maxima();
	}

}
