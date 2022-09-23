package org.minima.system.mds.runnable;

import org.minima.system.mds.handler.NETcommand;
import org.mozilla.javascript.Context;
import org.mozilla.javascript.Function;
import org.mozilla.javascript.NativeJSON;
import org.mozilla.javascript.Scriptable;

public class NETService {

	/**
	 * Which MiniDAPP does this Contect apply to
	 */
	String 		mMiniDAPPID;
	String 		mMiniDAPPName;
	
	/**
	 * JS Context and Scope
	 */
	Context mContext;
	Scriptable 	mScope;
	
	public NETService(String zMiniDAPPID, String zMiniName,  Context zContext, Scriptable zScope) {
		mMiniDAPPID		= zMiniDAPPID;
		mMiniDAPPName	= zMiniName;
		mContext 		= zContext;
		mScope 			= zScope;
	}
	
	public void GET(String zURL) {
		GET(zURL,null);
	}
	
	public void GET(String zURL, Function zCallback) {
		
		//Create a Command and run it..
		NETcommand net 	= new NETcommand(mMiniDAPPID, zURL);
		String result 	= net.runCommand();
		
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
