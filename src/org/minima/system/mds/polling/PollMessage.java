package org.minima.system.mds.polling;

import org.minima.utils.json.JSONObject;

public class PollMessage {

	String 		mTo;
	int 		mCounter;
	JSONObject 	mMessage;
	
//	public PollMessage(int zCounter, JSONObject zMessage) {
//		mCounter 	= zCounter;
//		mMessage 	= zMessage;
//		
//		//For everyone..
//		mTo 		= "*";
//	}
	
	public PollMessage(int zCounter, JSONObject zMessage, String zMiniDAPPID) {
		mCounter 	= zCounter;
		mMessage 	= zMessage;
		mTo 		= zMiniDAPPID;
	}
	
	public String getTo() {
		return mTo;
	}
	
	public int getCounter() {
		return mCounter;
	}
	
	public JSONObject getMessage() {
		return mMessage;
	}
	
	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		
		ret.put("to", getTo());
		ret.put("counter", getCounter());
		ret.put("message", getMessage());
		
		return ret;
	}
}
