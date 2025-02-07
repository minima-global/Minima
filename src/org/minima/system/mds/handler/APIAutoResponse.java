package org.minima.system.mds.handler;

import org.minima.system.mds.MDSManager;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class APIAutoResponse implements Runnable {

	MDSManager mMDS;
	
	//Is it a request or a response
	String mRandID;
	
	//From to
	String mFromMiniDAPPName;
	
	String mToMiniDAPPName;
	String mToMiniDAPPID;
	
	long mDelay = 10000;
	
	public APIAutoResponse(MDSManager zMDS, String zFromMiniDAPP, String zToMiniDAPP, String zToMiniDAPPID, String zRandID) {
		mMDS				= zMDS;
		mFromMiniDAPPName	= zFromMiniDAPP;
		mToMiniDAPPName		= zToMiniDAPP;
		mToMiniDAPPID		= zToMiniDAPPID;
		mRandID				= zRandID;
	}
	
	public void setImmediate() {
		mDelay = 0;
	}
	
	public void runauto() {
		Thread tt = new Thread(this);
		tt.start();
	}
	
	@Override
	public void run() {
		
		//Small Pause..
		try {Thread.sleep(mDelay);} catch (InterruptedException e) {}
		
		//The API message
		JSONObject apijson = new JSONObject();
		apijson.put("from", mFromMiniDAPPName);
		apijson.put("to", mToMiniDAPPName);
		apijson.put("id", mRandID);
		apijson.put("request", false);
		apijson.put("status", false);
		apijson.put("message", "API failed to respond..");
		
		//Post this message on the POLL Stack just for this one MininDAPP..
		JSONObject notify = new JSONObject();
		notify.put("event", "MDSAPI");
		notify.put("data", apijson);
		
		MinimaLogger.log("POST "+notify.toJSONString());
		
		//Tell the MDS..
		Message poll = new Message(MDSManager.MDS_POLLMESSAGE);
		poll.addObject("poll", notify);
		poll.addObject("to", mToMiniDAPPID);			
		mMDS.PostMessage(poll);
	}

}
