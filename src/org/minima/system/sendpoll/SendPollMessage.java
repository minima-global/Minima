package org.minima.system.sendpoll;

import org.minima.objects.base.MiniData;
import org.minima.utils.json.JSONObject;

public class SendPollMessage {

	String mUID;
	String mCommand;
	
	public SendPollMessage(String zCommand) {
		mUID 		= MiniData.getRandomData(16).to0xString();
		mCommand 	= zCommand;
	}
	
	public SendPollMessage(String zUID, String zCommand) {
		mUID 		= zUID;
		mCommand 	= zCommand;
	}
	
	public String getUID() {
		return mUID;
	}
	
	public String getCommand() {
		return mCommand;
	}
	
	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		
		ret.put("uid", getUID());
		ret.put("command", getCommand());
		
		return ret;
	}
	
	public SendPollMessage copy() {
		return new SendPollMessage(mUID, mCommand);
	}
}
