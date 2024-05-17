package org.minima.system.commands.maxima;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.StringTokenizer;

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
import org.minima.system.commands.CommandRunner;
import org.minima.system.network.maxima.MaxMsgHandler;
import org.minima.system.network.maxima.MaximaContactManager;
import org.minima.system.network.maxima.MaximaManager;
import org.minima.system.network.maxima.message.MaximaMessage;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class maxcontacts extends Command {

	public static int MAX_CONTACT_NUMBER = 30;
	
	public maxcontacts() {
		super("maxcontacts","[action:list|add|remove|search] (contact:) (id:) (publickey:) - Manage your Maxima contacts");
	}
	
	@Override
	public String getFullHelp() {
		return "\nmaxcontacts\n"
				+ "\n"
				+ "Manage your Maxima contacts. List, refresh, add, remove or search contacts.\n"
				+ "\n"
				+ "action:\n"
				+ "    list : List your Maxima contacts to see their id, address details, MLS and if they are on the same chain.\n"
				+ "    add : Add a new contact. Use with the 'contact' parameter.\n"
				+ "    remove : Remove a Maxima contact. Will also remove you from their contacts. Use with the 'id' parameter.\n"
				+ "    search : Search for a contact. Use with the 'id' or 'publickey' parameter.\n"
				+ "    export : Export a list of your contacts. Max addresses change constantly so you MUST import it quickly to your new node.\n"
				+ "    import : Import a list of your contacts.\n"
				+ "\n"
				+ "contact: (optional)\n"
				+ "    The Maxima contact address of another node. Can be found using the 'maxima' command.\n"
				+ "\n"
				+ "id: (optional)\n"
				+ "    The id of an existing contact to remove or search for.\n"
				+ "\n"
				+ "publickey: (optional)\n"
				+ "    The Maxima public key of an existing contact to remove or search for.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "maxcontacts\n"
				+ "\n"
				+ "maxcontacts action:list\n"
				+ "\n"
				+ "maxcontacts action:add contact:MxG18H..\n"
				+ "\n"
				+ "maxcontacts action:remove id:1\n"
				+ "\n"
				+ "maxcontacts action:export\n"
				+ "\n"
				+ "maxcontacts action:import contactlist:MxG1FE..,MxG1768..,etc\n"
				+ "\n"
				+ "maxcontacts action:search publickey:0x3081..\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","contact","id","publickey","enable","contactlist"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		MaximaManager max = Main.getInstance().getMaxima();
		if(max == null || !max.isInited()) {
			ret.put("status", false);
			ret.put("message", "Maxima still starting up..");
			return ret;
		}
		
		//Get the DB
		MaximaDB maxdb = MinimaDB.getDB().getMaximaDB();
		
		//What are we doing
		String func = getParam("action", "list");
		
		//Get the Tree tip - to check the contacts
		TxPoWTreeNode tip 	= MinimaDB.getDB().getTxPoWTree().getTip();
		MiniNumber topblock = MiniNumber.ZERO;
		if(tip != null) {
			topblock = tip.getBlockNumber();
		}
		 
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
				
				//Add some info..
				conjson.put("chaintip", topblock.toString());
				
				//Check it..
				if(tip == null) {
					conjson.put("samechain", false);
					
				}else {
					TxPoWTreeNode checknode = tip.getPastNode(checkblock);
					if(checknode != null) {
						MiniData nodehash		= checknode.getTxPoW().getTxPoWIDData();
						conjson.put("samechain", nodehash.isEqual(checkhash));
					}else {
						conjson.put("samechain", false);
					}
				}
				
				//And add..
				allcontacts.add(conjson);
			}
			
			details.put("allowallcontacts", max.getContactsManager().isAllowedAll());
			details.put("contacts", allcontacts);
			
		}else if(func.equals("add")) {
			
			//Are we allowing users.. ?
			if(!max.getContactsManager().isAllowedAll()) {
				ArrayList<String> allowed = max.getContactsManager().getAllowed();
				if(allowed.size()==0) {
					throw new CommandException("You have disabled adding contacts and have not allowed any public keys - use maxextra action:addallowed.. ");
				}
			}
			
			//Get the contact address
			String address 	= getParam("contact");

			//Is it a MAX address
			if(address.startsWith("MAX#")) {
				
				//Get the Address..
				JSONObject maxaddress = maxextra.getMaxAddress(address);
				
				//Get the address bit..
				if((boolean)maxaddress.get("success")) {
					
					JSONObject resp 	= (JSONObject)maxaddress.get("response");
					JSONObject mlsresp 	= (JSONObject)maxaddress.get("mlsresponse");
					
					//And finally..
					address = mlsresp.getString("address");
			
					MinimaLogger.log("ATTEMPT ADD MAX CONTACT : "+address);
					
				}else {
					throw new CommandException("MAX address invalid..");
				}
			}
			
			//Check is a valid address
			if(!maxextra.checkValidMxAddress(address)) {
				throw new CommandException("Invalid MX address : "+address);
			}
			
			//Check max number
			if(maxdb.getAllContacts().size()>=MAX_CONTACT_NUMBER) {
				throw new CommandException("Curently there is a limit of "+MAX_CONTACT_NUMBER+" contacts..");
			}
			
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
				MiniData maxpacket = MaxMsgHandler.constructMaximaData(sender);
				if(maxpacket == null) {
					throw new Exception("Could not build Maxima message in time..");
				}
				
				//And Send it..
				MiniData validresp = MaxMsgHandler.sendMaxPacket(tohost, toport, maxpacket);
				boolean valid = true;
				if(!validresp.isEqual(MaximaManager.MAXIMA_RESPONSE_OK)) {
					valid = false;
				}
				
				json.put("delivered", valid);
				if(!valid) {
					
					if(validresp.isEqual(MaximaManager.MAXIMA_RESPONSE_TOOBIG)) {
						json.put("error", "Maxima Mesasge too big");
					}else if(validresp.isEqual(MaximaManager.MAXIMA_RESPONSE_UNKNOWN)) {
						json.put("error", "Unkown Address");
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
			
		}else if(func.equals("export")) {
			
			//Export all the contacts..
			String outputlist ="";
			ArrayList<MaximaContact> contacts = maxdb.getAllContacts();
			for(MaximaContact contact : contacts) {
				String address = contact.getCurrentAddress();
				outputlist+=address+",";
			}
			
			//remove trailing ,
			if(outputlist.endsWith(",")) {
				outputlist = outputlist.substring(0,outputlist.length()-1);
			}
			
			details.put("contacts", contacts.size());
			details.put("contactlist", outputlist);
			details.put("message", "Maxima addresses change constantly - so import it immediately..");
			
		}else if(func.equals("import")) {
			
			String contactlist = getParam("contactlist");
			StringTokenizer strtok = new StringTokenizer(contactlist,",");
			JSONArray resarray = new JSONArray();
			while(strtok.hasMoreTokens()) {
				String contact = strtok.nextToken();
				contact = contact.replaceAll(" ", "");
				String command = "maxcontacts action:add contact:"+contact;
				
				JSONArray res 		= CommandRunner.getRunner().runMultiCommand(command);
				JSONObject result 	= (JSONObject) res.get(0);
				resarray.add(result);
			}
			
			details.put("size", resarray.size());
			details.put("contacts", resarray);
			
		}else {
			throw new CommandException("Unknown action : "+func);
		}
		
		ret.put("response", details);
		
		return ret;
	}
		
	@Override
	public Command getFunction() {
		return new maxcontacts();
	}

}
