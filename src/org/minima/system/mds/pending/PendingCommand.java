package org.minima.system.mds.pending;

import org.minima.objects.base.MiniData;
import org.minima.utils.json.JSONObject;

public class PendingCommand {

	String 		mUID;
	JSONObject 	mMiniDAPP;
	String 		mCommand;
	
	public PendingCommand(JSONObject zMiniDAPP, String zCommand) {
		mUID		= MiniData.getRandomData(16).to0xString();
		mMiniDAPP	= zMiniDAPP;
		mCommand 	= zCommand;
	}
	
	public String getUID() {
		return mUID;
	}
	
	public JSONObject getMiniDAPP() {
		return mMiniDAPP;
	}
	
	public String getCommand() {
		return mCommand;
	}
	
	public JSONObject toJSON() {
		JSONObject json = new JSONObject();
		json.put("uid", mUID);
		json.put("minidapp", mMiniDAPP);
		json.put("command", mCommand);
		return json;
	}
}
