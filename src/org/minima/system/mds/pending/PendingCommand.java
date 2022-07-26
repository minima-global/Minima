package org.minima.system.mds.pending;

import org.minima.objects.base.MiniData;
import org.minima.utils.json.JSONObject;

public class PendingCommand {

	String mUID;
	String mMiniDAPPID;
	String mCommand;
	
	public PendingCommand(String zMiniDAPPID, String zCommand) {
		mUID		= MiniData.getRandomData(16).to0xString();
		mMiniDAPPID = zMiniDAPPID;
		mCommand 	= zCommand;
	}
	
	public String getUID() {
		return mUID;
	}
	
	public String getMiniDAPPID() {
		return mMiniDAPPID;
	}
	
	public String getCommand() {
		return mCommand;
	}
	
	public JSONObject toJSON() {
		JSONObject json = new JSONObject();
		json.put("uid", mUID);
		json.put("minidappid", mMiniDAPPID);
		json.put("command", mCommand);
		return json;
	}
}
