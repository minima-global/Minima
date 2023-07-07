package org.minima.system.mds.runnable;

import org.minima.system.mds.MDSManager;
import org.minima.system.mds.handler.KEYPAIRcommand;
import org.mozilla.javascript.Context;
import org.mozilla.javascript.Function;
import org.mozilla.javascript.NativeJSON;
import org.mozilla.javascript.Scriptable;

public class KEYPAIRService {

	/**
	 * Which MiniDAPP does this command apply to
	 */
	String 		mMiniDAPPID;
	String 		mMiniDAPPName;
	
	/**
	 * MDS Manager
	 */
	MDSManager mMDS;
	
	/**
	 * JS Context and Scope
	 */
	Context mContext;
	Scriptable 	mScope;
	
	public KEYPAIRService(MDSManager zMDS, String zMiniDAPPID, String zMiniDAPPName, Context zContext, Scriptable zScope) {
		mMDS			= zMDS;
		mMiniDAPPID		= zMiniDAPPID;
		mMiniDAPPName	= zMiniDAPPName;
		mContext 		= zContext;
		mScope 			= zScope;
	}
	
	/**
	 * GET a value
	 */
	public void get(String zKey, Function zCallback) {
		
		//Create a Command and run it..
		KEYPAIRcommand kp 	= new KEYPAIRcommand(mMDS, mMiniDAPPID, KEYPAIRcommand.KEYPAIR_GET, zKey, "");
		String result 		= kp.runCommand();
		
		//Send Info Back
		if(zCallback == null) {
			return;
		}
		
		//The arguments
		Object[] args = { NativeJSON.parse(mContext, mScope, result, new NullCallable()) };
		
		//Call the main MDS Function in JS
		zCallback.call(mContext, mScope, mScope, args);
	}
	
	/**
	 * SET a value
	 */
	public void set(String zKey, String zValue) {
		set(zKey, zValue, null);
	}
	
	public void set(String zKey, String zValue, Function zCallback) {
		
		//Create a Command and run it..
		KEYPAIRcommand kp 	= new KEYPAIRcommand(mMDS, mMiniDAPPID, KEYPAIRcommand.KEYPAIR_SET, zKey, zValue);
		String result 		= kp.runCommand();
		
		//Send Info Back
		if(zCallback == null) {
			return;
		}
		
		//The arguments
		Object[] args = { NativeJSON.parse(mContext, mScope, result, new NullCallable()) };
		
		//Call the main MDS Function in JS
		zCallback.call(mContext, mScope, mScope, args);
	}
}