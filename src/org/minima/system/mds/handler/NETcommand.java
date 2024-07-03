package org.minima.system.mds.handler;

import org.minima.utils.RPCClient;
import org.minima.utils.json.JSONObject;

public class NETcommand {

	boolean mIsGet = true;
	
	String mMiniDAPPID;
	String mURL;
	String mPostData 	= "";
	String mReqType 	= "GET";
	String mBasicAuth	= "";
	
	public NETcommand(String zMiniDAPPID, String zURL) {
		mMiniDAPPID 	= zMiniDAPPID;
		mURL 			= zURL;
	}
	
	public NETcommand(String zMiniDAPPID, String zURL, String zData) {
		mMiniDAPPID 	= zMiniDAPPID;
		mURL 			= zURL;
		mPostData		= zData;
		mIsGet			= false;
		mReqType 		= "POST";
	}
	
	public NETcommand(String zMiniDAPPID, String zURL, String zData, String zBasicAuthorization) {
		mMiniDAPPID 	= zMiniDAPPID;
		mURL 			= zURL;
		mPostData		= "";
		mBasicAuth		= zBasicAuthorization;
	}
	
	public String runCommand() {
		
		//Default fail result
		JSONObject statfalse = new JSONObject();
		statfalse.put("request", mReqType);
		statfalse.put("url", mURL);
		statfalse.put("data", mPostData);
		statfalse.put("basicauth", mBasicAuth);
		statfalse.put("status", false);
		statfalse.put("pending", false);
		String result = statfalse.toJSONString();
		
		try {
			
			String resp = null;
			
			if(mIsGet) {
				
				//Is there an ATH token..
				if(mBasicAuth.equals("")) {
					//Run a GET request
					resp = RPCClient.sendGET(mURL);
				}else {
					//Run a GET request
					resp = RPCClient.sendGETAuth(mURL,mBasicAuth);
				}
				
			}else {
				//Run a POST request
				resp = RPCClient.sendPOST(mURL, mPostData);
			}
			
			JSONObject stattrue = new JSONObject();
			stattrue.put("request", mReqType);
			stattrue.put("url", mURL);
			stattrue.put("data", mPostData);
			stattrue.put("basicauth", mBasicAuth);
			stattrue.put("status", true);
			stattrue.put("pending", false);
			stattrue.put("response", resp);
			result = stattrue.toJSONString();
			
		}catch(Exception exc) {
			//MinimaLogger.log("ERROR NETcommand : "+mURL+" "+exc);
			
			statfalse.put("error", exc.toString());
			result = statfalse.toJSONString();
		}
		
		return result;
	}
	

}
