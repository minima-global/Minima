package org.minima.system.mds.runnable.api;

import org.mozilla.javascript.Function;

public class APICallback {

	String mRandID;
	
	Function mFunction;
	
	public APICallback(String zRandID, Function zFunction) {
		mRandID 	= zRandID;
		mFunction 	= zFunction;
	}
	
	public String getRandID() {
		return mRandID;
	}
	
	public Function getFunction() {
		return mFunction;
	}
}
