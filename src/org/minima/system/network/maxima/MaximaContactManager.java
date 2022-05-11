package org.minima.system.network.maxima;

import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;

public class MaximaContactManager extends MessageProcessor {

	public static final String MAXCONTACTS_MESSAGE = "MAXCONTACTS_MESSAGE";
	
	MaximaManager mManager;
	
	public MaximaContactManager(MaximaManager zManager) {
		super("MAXIMA_CONTACTS");
		
		mManager = zManager;
	}
	
	public JSONObject getContactInfo() {
		JSONObject ret = new JSONObject();
		
		ret.put("publickey", mManager.getPublicKey());
		ret.put("address", "");
		
		return ret;
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.getMessageType().equals(MAXCONTACTS_MESSAGE)) {
			
			//get the max json
			JSONObject maxjson = (JSONObject) zMessage.getObject("maxmessage");
			
			//Get the data
			
			MinimaLogger.log("Processing contact Meaxima Message.. "+maxjson.toString());
			
			//Process this special contacts message..
			
			
		}
		
	}

}
