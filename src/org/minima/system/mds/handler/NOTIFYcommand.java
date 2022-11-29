package org.minima.system.mds.handler;

import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class NOTIFYcommand {

	String mMiniDAPPID;
	String mName;
	String mText;
	boolean mShow;
	
	public NOTIFYcommand(String zMiniDAPPID, String zMiniDappName, String zText, boolean zShow) {
		mMiniDAPPID 	= zMiniDAPPID;
		mName			= zMiniDappName;
		mText			= zText;
		mShow			= zShow;
	}
	
	public String runCommand() {
		
		//Create a notification
		JSONObject notification = new JSONObject();
		notification.put("uid", mMiniDAPPID);
		notification.put("title", mName);
		notification.put("text", mText);
		notification.put("show", mShow);
		
		//Log it in the console
		MinimaLogger.log("Notification : "+notification.toString());
		
		//Post it
		Main.getInstance().PostNotifyEvent("NOTIFICATION", notification);
		
		JSONObject stattrue = new JSONObject();
		stattrue.put("command", "Notification");
		stattrue.put("status", true);
		stattrue.put("pending", false);
		stattrue.put("response", notification);
		String result = stattrue.toJSONString();
		
		return result;
	}
	

}
