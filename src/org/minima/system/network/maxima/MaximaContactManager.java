package org.minima.system.network.maxima;

import org.minima.database.MinimaDB;
import org.minima.database.maxima.MaximaContact;
import org.minima.database.maxima.MaximaDB;
import org.minima.database.maxima.MaximaHost;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.system.commands.network.maxima;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;

public class MaximaContactManager extends MessageProcessor {

	public static final String CONTACT_APPLICATION 		 = "**maxima_contact_ctrl**";
	
	public static final String MAXCONTACTS_RECMESSAGE 	 = "MAXCONTACTS_RECMESSAGE";
	public static final String MAXCONTACTS_UPDATEINFO 	 = "MAXCONTACTS_SENDMESSAGE";
	
	public static final String MAXCONTACTS_DELETECONTACT = "MAXCONTACTS_DELETECONTACT";
	
	MaximaManager mManager;
	
	public MaximaContactManager(MaximaManager zManager) {
		super("MAXIMA_CONTACTS");
		
		mManager = zManager;
	}
	
	public JSONObject getMaximaInfo(boolean zIntro) {
		JSONObject ret = new JSONObject();
		
		ret.put("intro", zIntro);
		ret.put("name", MinimaDB.getDB().getUserDB().getMaximaName());
		ret.put("publickey", mManager.getPublicKey().to0xString());
		ret.put("address", mManager.getRandomMaximaAddress());
		
		return ret;
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		//Get the DB
		MaximaDB maxdb = MinimaDB.getDB().getMaximaDB();
		
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
			
			//OK - lets get his current address
			boolean intro		= (boolean)contactjson.get("intro");
			String name 		= (String) contactjson.get("name");
			String address 		= (String) contactjson.get("address");
			
			//Create a Contact - if not there already
			MaximaContact checkcontact = maxdb.loadContactFromPublicKey(publickey);
			
			if(checkcontact == null) {
//				MinimaLogger.log("NEW CONTACT : "+contactjson.toString());
				
				MaximaContact mxcontact = new MaximaContact(name, publickey);
				mxcontact.setExtraData(new MiniData("0x00"));
				mxcontact.setCurrentAddress(address);
				mxcontact.setMyAddress("newcontact");
				
				maxdb.newContact(mxcontact);
				
			}else{
//				MinimaLogger.log("UPDATE CONTACT : "+contactjson.toString());
				
				MaximaContact mxcontact = new MaximaContact(name, publickey);
				mxcontact.setCurrentAddress(address);
				mxcontact.setExtraData(checkcontact.getExtraData());
				mxcontact.setMyAddress(checkcontact.getMyAddress());
				
				maxdb.updateContact(mxcontact);
			}
			
			//Send them a contact message aswell..
			if(intro || checkcontact == null) {
				Message msg = new Message(MAXCONTACTS_UPDATEINFO);
				msg.addString("publickey", publickey);
				msg.addString("address", address);
				PostMessage(msg);
			}
			
		}else if(zMessage.getMessageType().equals(MAXCONTACTS_UPDATEINFO)) {
			
			//Who To..
			String publickey = zMessage.getString("publickey");
			String address 	 = zMessage.getString("address");
			
			//Send a Contact info message to a user
			JSONObject contactinfo	= getMaximaInfo(false);
			
			//Now Update Our DB..
			MaximaContact mxcontact = maxdb.loadContactFromPublicKey(publickey);
			mxcontact.setMyAddress((String)contactinfo.get("address"));
			maxdb.updateContact(mxcontact);
			
			MiniString str			= new MiniString(contactinfo.toString());
			MiniData mdata 			= new MiniData(str.getData());
			
			//Now convert into the correct message..
			Message sender = maxima.createSendMessage(address, CONTACT_APPLICATION , mdata);
			
			//Post it on the stack
			mManager.PostMessage(sender);	
		
		}else if(zMessage.getMessageType().equals(MAXCONTACTS_DELETECONTACT)) {
			
			//Few steps here..
			
			
			
		}
		
	}

}
