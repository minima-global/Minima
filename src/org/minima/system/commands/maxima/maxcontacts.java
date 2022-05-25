package org.minima.system.commands.maxima;

import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.maxima.MaximaContact;
import org.minima.database.maxima.MaximaDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.network.maxima.MaximaContactManager;
import org.minima.system.network.maxima.MaximaManager;
import org.minima.system.network.maxima.message.MaximaMessage;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class maxcontacts extends Command {

	public maxcontacts() {
		super("maxcontacts","[action:list|myname|add|remove|search] (name:) (contact:) (id:) (publickey:) - Manage your Maxima contacts");
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
		
		//Get the Tree tip - to check the contacts
		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
		
		JSONObject details = new JSONObject();
		
		if(func.equals("list")) {
			
			//Get all the current contacts
			ArrayList<MaximaContact> contacts = maxdb.getAllContacts();
			JSONArray allcontacts = new JSONArray();
			for(MaximaContact contact : contacts) {
				
				//Check the tree tip
				JSONObject extradata = contact.getExtraData(); 
				MiniNumber checkblock = new MiniNumber((String)extradata.get("checkblock"));
				MiniData   checkhash  = new MiniData((String)extradata.get("checkhash"));
				
				//The contact
				JSONObject conjson = contact.toJSON();
				
				//Check it..
				TxPoWTreeNode checknode = tip.getPastNode(checkblock);
				if(checknode != null) {
					MiniData nodehash		= checknode.getTxPoW().getTxPoWIDData();
					conjson.put("samechain", nodehash.isEqual(checkhash));
				}else {
					conjson.put("samechain", false);
				}
				
				//And add..
				allcontacts.add(conjson);
			}
			details.put("contacts", allcontacts);
			
		}else if(func.equals("myname")) {
			
			String name = getParam("name");
			name = name.replace("\"", "");
			name = name.replace("'", "");
			name = name.replace(";", "");
			
			MinimaDB.getDB().getUserDB().setMaximaName(name);
			
			details.put("name", name);
			
			//Refresh
			max.PostMessage(MaximaManager.MAXIMA_REFRESH);
			
		}else if(func.equals("add")) {
			
			//Get the contact address
			String address 	= getParam("contact");

			//What data..
			JSONObject contactinfo 	= max.getContactsManager().getMaximaContactInfo(true,false);
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
				MiniData validresp = MaximaManager.sendMaxPacket(tohost, toport, maxpacket);
				boolean valid = true;
				if(!validresp.isEqual(MaximaManager.MAXIMA_RESPONSE_OK)) {
					valid = false;
				}
				
				json.put("delivered", valid);
				if(!valid) {
					
					if(validresp.isEqual(MaximaManager.MAXIMA_RESPONSE_TOOBIG)) {
						json.put("error", "Maxima Mesasge too big");
					}else if(validresp.isEqual(MaximaManager.MAXIMA_RESPONSE_UNKNOWN)) {
						json.put("error", "Unkonw Address");
					}else if(validresp.isEqual(MaximaManager.MAXIMA_RESPONSE_WRONGHASH)) {
						json.put("error", "TxPoW Hash wrong");
					}else {
						json.put("error", "Not delivered");
					}
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
			
			details.put("removed", id);
		
		}else if(func.equals("search")) {
			
			MaximaContact chosen = null;
			
			if(existsParam("publickey")) {
				String pubkey = getParam("publickey");
				ArrayList<MaximaContact> contacts = maxdb.getAllContacts();
				for(MaximaContact contact : contacts) {
					if(contact.getPublicKey().equals(pubkey)) {
						chosen = contact;
						break;
					}
				}
				
			}else if(existsParam("id")) {
				int id = getNumberParam("id").getAsInt();
				ArrayList<MaximaContact> contacts = maxdb.getAllContacts();
				for(MaximaContact contact : contacts) {
					if(contact.getUID() == id) {
						chosen = contact;
						break;
					}
				}
				
			}else{
				throw new CommandException("Must specify id or publickey");
			}
			
			if(chosen == null) {
				throw new CommandException("MaxContact not found");
			}
			
			details.put("contact", chosen.toJSON());
			
		}else {
			throw new CommandException("Unknonw action : "+func);
		}
		
		ret.put("response", details);
		
		return ret;
	}
		
	@Override
	public Command getFunction() {
		return new maxcontacts();
	}

}
