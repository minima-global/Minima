package org.minima.system.commands.network;

import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.maxima.MaximaDB;
import org.minima.database.maxima.MaximaHost;
import org.minima.objects.Address;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.network.maxima.MaximaManager;
import org.minima.system.network.maxima.message.MaximaMessage;
import org.minima.system.params.GeneralParams;
import org.minima.utils.Crypto;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class maxima extends Command {

	public maxima() {
		super("maxima","[action:info|send] (to:) (application:) (data:) (logs:true|false) - Check your Maxima details, send a message / data, enable logs");
	}
	
	@Override
	public String getFullHelp() {
		return "Maxima is an information transport layer running ontop of Minima";
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		String func = getParam("action", "info");
		
		MaximaManager max = Main.getInstance().getMaxima();
		if(!max.isInited()) {
			ret.put("status", false);
			ret.put("message", "Maxima still starting up..");
			return ret;
		}
		
		MaximaDB maxdb = MinimaDB.getDB().getMaximaDB();
		
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
			
			String fullhost = GeneralParams.MINIMA_HOST+":"+GeneralParams.MINIMA_PORT;
			
			//Show details
			details.put("identity", max.getMaximaIdentity());
			details.put("localhost", fullhost);
			details.put("localidentity", max.getMaximaIdentity()+"@"+fullhost);
			details.put("logs", max.mMaximaLogs);
			
			//Get all the current hosts
			ArrayList<MaximaHost> hosts = maxdb.getAllHosts();
			JSONArray allhosts = new JSONArray();
			for(MaximaHost host : hosts) {
				allhosts.add(host.toJSON());
			}
			details.put("hosts", allhosts);
			
			//Get all the current contacts
			//..
			
			ret.put("response", details);
		
		}else if(func.equals("new")) {
			
			throw new CommandException("Supported Soon..");
			
//			//Create a new Maxima Identity..
//			max.createMaximaKeys();
//			
//			//Show details
//			String ident = max.getMaximaIdentity();  
//			details.put("identity", ident);
//			ret.put("response", details);

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
			if(publickey.startsWith("Mx")) {
				publickey = Address.convertMinimaAddress(publickey).to0xString();
			}
			
			//get the host and port..
			String tohost 	= fullto.substring(indexp+1,index);
			int toport		= Integer.parseInt(fullto.substring(index+1));	
			
			//Which application
			String application 	= getParam("application");
			
			MiniData mdata 	= null;
			if(isParamJSONObject("data")) {
			
				MiniString datastr = new MiniString(getJSONObjectParam("data").toString());
				mdata = new MiniData(datastr.getData());
			}else {
				
				mdata = getDataParam("data");
			} 
			
			//Get the complete details..
			MaximaMessage maxmessage = max.createMaximaMessage(publickey, application, mdata);
			
			//Get the MinData version
			MiniData maxdata = MiniData.getMiniDataVersion(maxmessage);
			
			//Hash it..
			MiniData hash 	= Crypto.getInstance().hashObject(maxdata);
			
			//Convert to JSON
			JSONObject json = maxmessage.toJSON();
			json.put("msgid", hash.to0xString());
			
			//Send to Maxima..
			Message sender = new Message(MaximaManager.MAXIMA_SENDMESSAGE);
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
