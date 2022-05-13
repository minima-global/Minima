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

public class maxcontacts extends Command {

	public maxcontacts() {
		super("maxcontacts","[action:list|myname|add|remove] (name:) (address:) (publickey:) - Manage your Maxima contacts");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		MaximaManager max = Main.getInstance().getMaxima();
		if(!max.isInited()) {
			ret.put("status", false);
			ret.put("message", "Maxima still starting up..");
			return ret;
		}
		
		//Get the DB
		MaximaDB maxdb = MinimaDB.getDB().getMaximaDB();
		
		//What are we doing
		String func = getParam("action", "list");
		
		
		JSONObject details = new JSONObject();
		
		if(func.equals("list")) {
			
			//Get all the current contacts
			ArrayList<MaximaContact> contacts = maxdb.getAllContacts();
			JSONArray allcontacts = new JSONArray();
			for(MaximaContact contact : contacts) {
				allcontacts.add(contact.toJSON());
			}
			details.put("contacts", allcontacts);
			
		}else if(func.equals("myname")) {
			
			String name = getParam("name");
			
			MinimaDB.getDB().getUserDB().setMaximaName(name);
			
			details.put("name", name);
			
		}else if(func.equals("add")) {
			
			//Get the contact address
			String address 	= getParam("address");

			//What data..
			JSONObject contactinfo 	= max.getContactsManager().getMaximaInfo(true);
			MiniString datastr 		= new MiniString(contactinfo.toString());
			MiniData mdata 			= new MiniData(datastr.getData());
			
			//Now convert into the correct message..
			Message sender = maxima.createSendMessage(address, MaximaContactManager.CONTACT_APPLICATION, mdata);
			
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
			
			details.put("maxima", json);
		
		}else if(func.equals("remove")) {
			
			//Get the id
			String id = getParam("id");
			
			//And send a message to sort this out
			Message remove = new Message(MaximaContactManager.MAXCONTACTS_DELETECONTACT);
			remove.addInteger("id", Integer.parseInt(id));
			max.getContactsManager().PostMessage(remove);
			
			details.put("contact", id);
		}
		
		ret.put("response", details);
		
		return ret;
	}
		
	@Override
	public Command getFunction() {
		return new maxcontacts();
	}

}
