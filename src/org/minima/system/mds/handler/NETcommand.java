package org.minima.system.mds.handler;

import org.minima.system.params.GeneralParams;
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
		
		//Is it a blocked Host!
		if(isBlockedURL(mURL)) {
			statfalse.put("error", "Blocked host URL.. use -allowallip to enable this host : "+mURL);
			return statfalse.toJSONString();
		}
		
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
	
	public static boolean isBlockedURL(String url) {
	    try {
	    	
	    	//Do we allow ALL IP
	    	if(GeneralParams.ALLOW_ALL_IP) {
	    		return false;
	    	}
	    	
	    	//Make sure is an http / https - not file..
	    	if(!url.toLowerCase().startsWith("http")) {
	    		return true;
	    	}
	    	
	    	if(url.toLowerCase().contains("localhost")) {
	    		return true;
	    	}
	    	
	        java.net.URL parsed = new java.net.URL(url);
	        String host 		= parsed.getHost();

	        
	        
	        // Resolve hostname to IP
	        java.net.InetAddress addr = java.net.InetAddress.getByName(host);

	        // Block private, link-local, loopback, multicast
	        if (addr.isLoopbackAddress()       // 127.0.0.0/8
	            || addr.isLinkLocalAddress()    // 169.254.0.0/16
	            || addr.isSiteLocalAddress()    // 10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16
	            || addr.isAnyLocalAddress()     // 0.0.0.0
	            || addr.isMulticastAddress()) { // 224.0.0.0/4
	            return true;
	        }

	        // Block cloud metadata IPs
	        byte[] bytes = addr.getAddress();
	        if (bytes[0] == (byte)169 && bytes[1] == (byte)254) return true;

	        // Block known metadata hostnames
	        if (host.equals("metadata.google.internal")) return true;
	        if (host.endsWith(".internal")) return true;

	        return false;
	    } catch (Exception e) {
	        return true; // Block on error — fail closed
	    }
	}

}
