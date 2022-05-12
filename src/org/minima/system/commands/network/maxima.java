package org.minima.system.commands.network;

import java.util.ArrayList;
import java.util.Random;

import org.minima.database.MinimaDB;
import org.minima.database.maxima.MaximaContact;
import org.minima.database.maxima.MaximaDB;
import org.minima.database.maxima.MaximaHost;
import org.minima.objects.Address;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.network.maxima.MaximaContactManager;
import org.minima.system.network.maxima.MaximaManager;
import org.minima.system.network.maxima.message.MaximaMessage;
import org.minima.system.params.GeneralParams;
import org.minima.utils.Crypto;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class maxima extends Command {

	public maxima() {
		super("maxima","[action:info|send|contact] (contact:) (to:) (application:) (data:) (logs:true|false) - Check your Maxima details, send a message / data, enable logs");
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
			
			//Your local IP address
			String fullhost = GeneralParams.MINIMA_HOST+":"+GeneralParams.MINIMA_PORT;
			
			
			//Show details
			details.put("logs", max.mMaximaLogs);
			details.put("publickey", max.getPublicKey().to0xString());
			details.put("localhost", fullhost);
			details.put("localidentity", max.getMaximaIdentity()+"@"+fullhost);
			details.put("contact", max.getLocalMaximaAddress());
			
			//Add ALL Hosts
			ArrayList<MaximaHost> hosts = maxdb.getAllHosts();
			JSONArray allhosts = new JSONArray();
			for(MaximaHost host : hosts) {
				allhosts.add(host.toJSON());
			}
			details.put("hosts", allhosts);
			
			//Get all the current contacts
			ArrayList<MaximaContact> contacts = maxdb.getAllContacts();
			JSONArray allcontacts = new JSONArray();
			for(MaximaContact contact : contacts) {
				allcontacts.add(contact.toJSON());
			}
			details.put("contacts", allcontacts);
			
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
			
			//Which application
			String application 	= getParam("application");

			//What data
			MiniData mdata 	= null;
			if(isParamJSONObject("data")) {
				MiniString datastr = new MiniString(getJSONObjectParam("data").toString());
				mdata = new MiniData(datastr.getData());
			}else {
				mdata = getDataParam("data");
			} 
			
			//Now convert into the correct message..
			Message sender = createSendMessage(fullto, application, mdata);
			
			//Get the message
			MaximaMessage maxmessage = (MaximaMessage) sender.getObject("maxima");
			
			//Convert to JSON
			JSONObject json = maxmessage.toJSON();
			json.put("msgid", sender.getString("msgid"));
			
			String tohost 	= sender.getString("tohost");
			int toport 		= sender.getInteger("toport");
			
			//Now construct a complete Maxima Data packet
			try {
				//Create the packet
				MiniData maxpacket = MaximaManager.constructMaximaData(sender);
			
				//And Send it..
				boolean valid = MaximaManager.sendMaxPacket(tohost, toport, maxpacket);
				json.put("delivered", valid);
				if(!valid) {
					json.put("error", "Not delivered");
				}
				
			}catch(Exception exc){
				//Something wrong
				json.put("delivered", false);
				json.put("error", exc.toString());
			}
			
			//Post It!
//			max.PostMessage(sender);
			
			ret.put("response", json);
		
		}else if(func.equals("contact")) {
			
			//Get the contact address
			String contact = getParam("contact");

			//What data..
			JSONObject contactinfo 	= max.getContactsManager().getMaximaInfo();
			MiniString datastr 		= new MiniString(contactinfo.toString());
			MiniData mdata 			= new MiniData(datastr.getData());
			
			//Now convert into the correct message..
			Message sender = createSendMessage(contact, MaximaContactManager.CONTACT_APPLICATION, mdata);
			
			//Get the message
			MaximaMessage maxmessage = (MaximaMessage) sender.getObject("maxima");
			
			//Who to..
			String tohost 	= sender.getString("tohost");
			int toport 		= sender.getInteger("toport");
			
			//Now construct a complete Maxima Data packet
			JSONObject json = maxmessage.toJSON();
			json.put("msgid", sender.getString("msgid"));
			try {
				//Create the packet
				MiniData maxpacket = MaximaManager.constructMaximaData(sender);
			
				//And Send it..
				boolean valid = MaximaManager.sendMaxPacket(tohost, toport, maxpacket);
				json.put("delivered", valid);
				if(!valid) {
					json.put("error", "Not delivered");
				}
				
			}catch(Exception exc){
				//Something wrong
				json.put("delivered", false);
				json.put("error", exc.toString());
			}
			
			ret.put("response", json);
		}
		
		return ret;
	}
	
	public static Message createSendMessage(String zFullTo, String zApplication, MiniData zData) {
		
		MaximaManager max = Main.getInstance().getMaxima();
		
		//Send a message..
		String fullto 	= zFullTo;
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
		
		//Get the complete details..
		MaximaMessage maxmessage = max.createMaximaMessage(publickey, zApplication, zData);
		
		//Get the MinData version
		MiniData maxdata = MiniData.getMiniDataVersion(maxmessage);
		
		//Hash it..
		MiniData hash 	= Crypto.getInstance().hashObject(maxdata);
		
		//Send to Maxima..
		Message sender = new Message(MaximaManager.MAXIMA_SENDMESSAGE);
		sender.addObject("maxima", maxmessage);
		
		sender.addObject("mypublickey", max.getPublicKey());
		sender.addObject("myprivatekey", max.getPrivateKey());
		
		sender.addString("publickey", publickey);
		sender.addString("tohost", tohost);
		sender.addInteger("toport", toport);
		
		sender.addString("msgid", hash.to0xString());
		
		return sender;
	}
	
	
	@Override
	public Command getFunction() {
		return new maxima();
	}

}
