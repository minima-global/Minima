package org.minima.system.mds.handler;

import org.minima.system.mds.MDSManager;
import org.minima.utils.json.JSONObject;

public class KEYPAIRcommand {

	/**
	 * Base FILE Functions
	 */
	public static final String KEYPAIR_SET 		= "KEYPAIR_SET";
	public static final String KEYPAIR_GET 		= "KEYPAIR_GET";
	
	/**
	 * Main MDS Manager
	 */
	MDSManager mMDS;
	
	/**
	 * Which MiniDAPP os this
	 */
	String mMiniDAPPID;
	
	String mKeyPairCommand;
	
	String mKey;
	String mValue;
	
	public KEYPAIRcommand(MDSManager zManager, String zMiniDAPPID, String zCommand, String zKey, String zValue) {
		mMDS			= zManager;
		mMiniDAPPID 	= zMiniDAPPID;
		mKeyPairCommand	= zCommand;
		mKey			= zKey;
		mValue			= zValue;
	}
	
	public String runCommand() {
		
		if(mKeyPairCommand.equals(KEYPAIR_GET)) {
			
			//The result
			JSONObject stat = new JSONObject();
			stat.put("command", mKeyPairCommand);
			stat.put("key", mKey);
			
			//Get a Value..
			String value = mMDS.getMDSKeyPair(mMiniDAPPID, mKey);
			stat.put("value", value);
			stat.put("status", true);
			
			//False
			if(value == null) {
				stat.put("status", false);
			}
			
			stat.put("pending", false);
			
			return stat.toString();
		
		}else{
			
			//The result
			JSONObject stat = new JSONObject();
			stat.put("command", mKeyPairCommand);
			stat.put("key", mKey);
			stat.put("value", mValue);
			stat.put("status", true);
			stat.put("pending", false);
			
			//Set a Value..
			mMDS.setMDSKeyPair(mMiniDAPPID, mKey, mValue);
			
			return stat.toString();
		}
	}
}
