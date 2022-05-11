package org.minima.system.network.maxima;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.system.commands.network.maxima;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;

public class MaximaContactManager extends MessageProcessor {

	public static final String MAXCONTACTS_RECMESSAGE 	= "MAXCONTACTS_RECMESSAGE";
	public static final String MAXCONTACTS_SENDMESSAGE 	= "MAXCONTACTS_SENDMESSAGE";
	
	MaximaManager mManager;
	
	public MaximaContactManager(MaximaManager zManager) {
		super("MAXIMA_CONTACTS");
		
		mManager = zManager;
	}
	
	public JSONObject getContactInfo(boolean intro) {
		JSONObject ret = new JSONObject();
		
		ret.put("publickey", mManager.getPublicKey().to0xString());
		ret.put("address", mManager.getRandomHostAddress());
		ret.put("intro", intro);
		
		return ret;
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.getMessageType().equals(MAXCONTACTS_RECMESSAGE)) {
			
			//get the max json
			JSONObject maxjson = (JSONObject) zMessage.getObject("maxmessage");
			
			//Get the public key
			String publickey = (String) maxjson.get("from");
			
			//Get the data
			String data 	= (String) maxjson.get("data");
			MiniData dat 	= new MiniData(data);
			
			//Convert to a JSON
			MiniString datastr 		= new MiniString(dat.getBytes());
			JSONObject contactjson 	= (JSONObject) new JSONParser().parse(datastr.toString());
			
			//Process this special contacts message..
			String contactkey = (String) contactjson.get("publickey"); 
			if(!contactkey.equals(publickey)) {
				MinimaLogger.log("Received contact message with mismatch public keys..");
				return;
			}
			
			MinimaLogger.log("CONTACT DATA : "+contactjson.toString());
			
			//OK - lets get his current address
			String address 	= (String) contactjson.get("address");
			boolean intro 	= (boolean) contactjson.get("intro");
			
			//Update the DB..
			//..
			
			
			//Send them a contact message aswell..
			if(intro) {
			
				//What data..
				JSONObject contactinfo 	= getContactInfo(false);
				MiniData mdata 			= new MiniData(new MiniString(contactinfo.toString()).getData());
				
				//Now convert into the correct message..
				Message sender = maxima.createSendMessage(address, "**contact_ctrl**", mdata);
				
				//Get the message
				String tohost 			 = sender.getString("tohost");
				int toport 				 = sender.getInteger("toport");
				
				//Create the packet
				MiniData maxpacket = MaximaManager.constructMaximaData(sender);
				
				//Post it on the stack
				mManager.PostMessage(sender);
				
			}
		}
		
	}

}
