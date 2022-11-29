package org.minima.system.mds.runnable;

import org.minima.system.mds.MDSManager;
import org.minima.system.mds.handler.COMMSCommand;
import org.mozilla.javascript.Context;
import org.mozilla.javascript.Function;
import org.mozilla.javascript.NativeJSON;
import org.mozilla.javascript.Scriptable;

public class COMMSService {

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
	
	public COMMSService(MDSManager zMDS, String zMiniDAPPID, String zMiniDAPPName, Context zContext, Scriptable zScope) {
		mMDS			= zMDS;
		mMiniDAPPID		= zMiniDAPPID;
		mMiniDAPPName	= zMiniDAPPName;
		mContext 		= zContext;
		mScope 			= zScope;
	}
	
	/**
	 * Send a PUBLIC message
	 */
	public void broadcast(String zMessage) {
		broadcast(zMessage,null);
	}
	
	public void broadcast(String zMessage, Function zCallback) {
		
		//Create a Command and run it..
		COMMSCommand comms = new COMMSCommand(mMDS, "*", mMiniDAPPName, zMessage);
		String result 		= comms.runCommand();
		
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
	 * Send a PRIVATE message
	 */
	public void solo(String zMessage) {
		solo(zMessage,null);
	}
	
	public void solo(String zMessage, Function zCallback) {
		
		//Create a Command and run it..
		COMMSCommand comms = new COMMSCommand(mMDS, mMiniDAPPID, mMiniDAPPName, zMessage);
		String result 		= comms.runCommand();
		
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
