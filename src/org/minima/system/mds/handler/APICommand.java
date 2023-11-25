package org.minima.system.mds.handler;

import org.minima.system.mds.MDSManager;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class APICommand {

	MDSManager mMDS;
	
	//Is it a request or a response
	boolean mRequest;
	String mRandID;
	
	//From to
	String mFromMiniDAPPName;
	
	String mToMiniDAPPName;
	String mToMiniDAPPID;
	
	String mMessage;
	
	public APICommand(MDSManager zMDS, String zFromMiniDAPP, String zToMiniDAPP, 
			String zToMiniDAPPID, String zMessage, String zRandID, boolean zRequest) {
		
		mMDS				= zMDS;
		mFromMiniDAPPName	= zFromMiniDAPP;
		mToMiniDAPPName		= zToMiniDAPP;
		mToMiniDAPPID		= zToMiniDAPPID;
		mMessage			= zMessage;
		mRandID				= zRandID;
		mRequest			= zRequest;
	}
	
	public String runCommand() {
		
		//The API message
		JSONObject apijson = new JSONObject();
		apijson.put("from", mFromMiniDAPPName);
		apijson.put("to", mToMiniDAPPName);
		apijson.put("id", mRandID);
		apijson.put("request", mRequest);
		apijson.put("message", mMessage);
		apijson.put("status", true);
		
		//Post this message on the POLL Stack just for this one MininDAPP..
		JSONObject notify = new JSONObject();
		notify.put("event", "MDSAPI");
		notify.put("data", apijson);
		
		//Tell the MDS..
		Message poll = new Message(MDSManager.MDS_POLLMESSAGE);
		poll.addObject("poll", notify);
		poll.addObject("to", mToMiniDAPPID);			
		mMDS.PostMessage(poll);
		
		//The result
		JSONObject stattrue = new JSONObject();
		stattrue.put("status", true);
		stattrue.put("pending", false);
		stattrue.put("response", apijson);
		String result = stattrue.toJSONString();
		
		return result;
	}
}