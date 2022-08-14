package org.minima.system.mds.polling;

import org.minima.utils.json.JSONObject;

public class PollMessage {

	int mCounter;
	
	JSONObject mMessage;
	
	public PollMessage(int zCounter, JSONObject zMessage) {
		mCounter = zCounter;
		mMessage = zMessage;
	}
	
	public int getCounter() {
		return mCounter;
	}
	
	public JSONObject getMessage() {
		return mMessage;
	}
	
	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		
		ret.put("counter", getCounter());
		ret.put("message", getMessage());
		
		return ret;
	}
}
