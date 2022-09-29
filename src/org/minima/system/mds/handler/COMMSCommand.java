package org.minima.system.mds.handler;

import org.minima.system.mds.MDSManager;
import org.minima.utils.MinimaLogger;
import org.minima.utils.RPCClient;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class COMMSCommand {

	MDSManager mMDS;
	
	String mMiniDAPPID;
	String mMessage;
	
	public COMMSCommand(MDSManager zMDS, String zMiniDAPPID, String zMessage) {
		mMDS			= zMDS;
		mMiniDAPPID 	= zMiniDAPPID;
		mMessage		= zMessage;
	}
	
	public String runCommand() {
		
		//Post this message on the POLL Stack just for this one MininDAPP..
		JSONObject notify = new JSONObject();
		notify.put("event", "MDSCOMMS");
		notify.put("data", mMessage);
		
		//Tell the MDS..
		Message poll = new Message(MDSManager.MDS_POLLMESSAGE);
		poll.addObject("poll", notify);
		poll.addObject("to", mMiniDAPPID);			
		mMDS.PostMessage(poll);
		
		JSONObject stattrue = new JSONObject();
		stattrue.put("message", mMessage);
		stattrue.put("status", true);
		stattrue.put("pending", false);
		String result = stattrue.toJSONString();
		
		return result;
	}
	

}
