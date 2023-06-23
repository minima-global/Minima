package org.minima.system.mds.pending;

import java.util.Date;

import org.minima.objects.base.MiniData;
import org.minima.utils.json.JSONObject;

public class PendingCommand {

	String 		mUID;
	JSONObject 	mMiniDAPP;
	String 		mCommand;
	long 		mDate;
	
	public PendingCommand(JSONObject zMiniDAPP, String zCommand) {
		mUID		= MiniData.getRandomData(16).to0xString();
		mMiniDAPP	= zMiniDAPP;
		mCommand 	= zCommand;
		mDate		= System.currentTimeMillis();
	}
	
	public String getUID() {
		return mUID;
	}
	
	public JSONObject getMiniDAPP() {
		return mMiniDAPP;
	}
	
	public long getDate() {
		return mDate;
	}
	
	public String getCommand() {
		return mCommand;
	}
	
	public JSONObject toJSON() {
		JSONObject json = new JSONObject();
		json.put("uid", mUID);
		json.put("minidapp", mMiniDAPP);
		json.put("date", new Date(mDate).toString());
		json.put("command", mCommand);
		return json;
	}
}
