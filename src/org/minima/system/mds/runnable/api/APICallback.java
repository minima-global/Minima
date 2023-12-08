package org.minima.system.mds.runnable.api;

import org.mozilla.javascript.Context;
import org.mozilla.javascript.Function;
import org.mozilla.javascript.Scriptable;

public class APICallback {

	/**
	 * JS Context and Scope
	 */
	Context mContext;
	Scriptable 	mScope;
	
	String mRandID;
	Function mFunction;
	
	public APICallback(Context zContext, Scriptable zScope, String zRandID, Function zFunction) {
		mContext	= zContext;
		mScope		= zScope;
		mRandID 	= zRandID;
		mFunction 	= zFunction;
	}
	
	public Context getContext() {
		return mContext;
	}
	
	public Scriptable getScope() {
		return mScope;
	}
	
	public String getRandID() {
		return mRandID;
	}
	
	public Function getFunction() {
		return mFunction;
	}
}
