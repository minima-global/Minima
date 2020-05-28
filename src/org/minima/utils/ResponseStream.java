package org.minima.utils;

import org.minima.utils.json.JSONObject;

public class ResponseStream {

	/**
	 * Maximum Amount of time to wait for a response finish
	 */
	public static final long MAX_WAITTIME = 20000;
	
	/**
	 * What is the response
	 */
	String mFunction = "";
	
	/**
	 * The response as a JSON
	 */
	JSONObject mJSON = new JSONObject();
	
	/**
	 * The actual data being sent back
	 */
	JSONObject mDataJSON = new JSONObject();
	
	/**
	 * Has it finished
	 */
	boolean mFinished     = false;
	
	/**
	 * The final returned response
	 */
	String mFinalResponse = "";
		
	/**
	 * The LOCK Object
	 */
	Object mLock = new Object();
	
	/**
	 * Main Constructor
	 */
	public ResponseStream(){
		//Not finished yet
		mFinished = false;
	}
	
	public void setFunction(String zFunction) {
		mFunction = zFunction;
	}
	
	public String getResponse() {
		return mFinalResponse;
	}
	
	public JSONObject getDataJSON() {
		return mDataJSON;
	}
	
	public JSONObject getFinalJSON() {
		return mJSON;
	}
	
	public void endStatus(boolean zValid, String zMessage) {
		mJSON.put("status", zValid);
		mJSON.put("minifunc", mFunction);
		mJSON.put("message", zMessage);
		
		//Add the data to the JSON
		mJSON.put("response", mDataJSON);
	
		//Create the final response
		mFinalResponse = mJSON.toString().replaceAll("\\\\/", "/");
		
		//It's finished
		setFinished();
	}
	
	public void hardEndStatus(String zResult) {
		//Hard set the response..
		mFinalResponse = zResult;
		
		//It's finished
		setFinished();
	}
	
	private void setFinished() {
		synchronized (mLock) {
			mFinished = true;
			mLock.notifyAll();
		}
	}
	
	/**
	 * Wait a maximum amount of time..
	 */
	public void waitToFinish() {
		synchronized (mLock) {
			if(mFinished) {
				return;	
			}
			
			try {
				mLock.wait(MAX_WAITTIME);
			} catch (InterruptedException e) {}
		}
		
		//Are we finished..
		if(!mFinished) {
			endStatus(false, "Operation Timed out..");
		}
	}
}
