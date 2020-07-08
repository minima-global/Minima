package org.minima.system.network.commands;

import org.minima.system.network.minidapps.minilib.JSMiniLibUtil;
import org.mozilla.javascript.Context;
import org.mozilla.javascript.Function;
import org.mozilla.javascript.Scriptable;

public class NET implements Runnable {

	//The Command to run
	String mCommand;
	String mMiniDAPPID;
	
	//Call back with the response when finished
	Function   mCallback;
	Context    mContext;
	Scriptable mScope;
	
	//The Final Result..
	String mFinalResult = "";
	
	public NET(String zCommand, String zMiniDAPPID) {
		this(zCommand, zMiniDAPPID,null,null,null);
	}
	
	public NET(String zCommand, String zMiniDAPPID, Function zCallback, Context zContext, Scriptable zScope) {
		mCommand    = zCommand;
		mMiniDAPPID = zMiniDAPPID;
		mCallback   = zCallback;
		mContext    = zContext;
		mScope      = zScope;
	}

	public String getFinalResult() {
		return mFinalResult;
	}

	@Override
	public void run() {
				
		
		
		
		//Now send the result back vis the callback..
		if(mCallback != null) {
			//Create a native JSON
			Object json = JSMiniLibUtil.makeJSONObject(mFinalResult, mContext, mScope);
			
			//Make a function variable list
			Object functionArgs[] = { json };
		    
			//Call the function..
			mCallback.call(mContext, mScope, mScope, functionArgs);
		}
	}

	
}
