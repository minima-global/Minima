package org.minima.system.mds.handler;

import org.minima.utils.MinimaLogger;
import org.minima.utils.RPCClient;
import org.minima.utils.json.JSONObject;

public class NETcommand {

	String mMiniDAPPID;
	String mURL;
	
	public NETcommand(String zMiniDAPPID, String zURL) {
		mMiniDAPPID 	= zMiniDAPPID;
		mURL 			= zURL;
	}
	
	public String runCommand() {
		
		//Default fail result
		JSONObject statfalse = new JSONObject();
		statfalse.put("url", mURL);
		statfalse.put("status", false);
		statfalse.put("pending", false);
		String result = statfalse.toJSONString();
		
		try {
			
			//Run an RPC request
			String resp = RPCClient.sendGET(mURL);
		
			JSONObject stattrue = new JSONObject();
			stattrue.put("url", mURL);
			stattrue.put("status", true);
			stattrue.put("pending", false);
			stattrue.put("response", resp);
			result = stattrue.toJSONString();
			
		}catch(Exception exc) {
			MinimaLogger.log("ERROR NETHANDLER : "+mURL+" "+exc);
			
			statfalse.put("error", exc.toString());
			result = statfalse.toJSONString();
		}
		
		return result;
	}
	

}
