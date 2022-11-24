package org.minima.system.mds.handler;

import org.minima.system.mds.MDSManager;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class COMMSCommand {

	MDSManager mMDS;
	
	String mMiniDAPPID;
	String mMiniDAPPName;
	String mMessage;
	
	public COMMSCommand(MDSManager zMDS, String zMiniDAPPID, String zMiniDAPPName, String zMessage) {
		mMDS			= zMDS;
		mMiniDAPPID 	= zMiniDAPPID;
		mMiniDAPPName	= zMiniDAPPName;
		mMessage		= zMessage;
	}
	
	public String runCommand() {
		
		//The COMMS message
		JSONObject comms = new JSONObject();
		comms.put("minidapp", mMiniDAPPName);
		comms.put("public", mMiniDAPPID.equals("*"));
		comms.put("message", mMessage);
		
		//Post this message on the POLL Stack just for this one MininDAPP..
		JSONObject notify = new JSONObject();
		notify.put("event", "MDSCOMMS");
		notify.put("data", comms);
		
		//Tell the MDS..
		Message poll = new Message(MDSManager.MDS_POLLMESSAGE);
		poll.addObject("poll", notify);
		poll.addObject("to", mMiniDAPPID);			
		mMDS.PostMessage(poll);
		
		//The result
		JSONObject stattrue = new JSONObject();
		stattrue.put("status", true);
		stattrue.put("pending", false);
		stattrue.put("response", comms);
		String result = stattrue.toJSONString();
		
		return result;
	}
	

}
