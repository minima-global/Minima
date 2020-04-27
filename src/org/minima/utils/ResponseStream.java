package org.minima.utils;

import org.minima.utils.json.JSONObject;

public class ResponseStream {

	/**
	 * Maximum Amount of time to wait for a response finish
	 */
	public static final long MAX_WAITTIME = 10000;
	
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
	 * Is this the LOCAL machine..
	 */
	boolean mIsLocal = false;
	
	/**
	 * Main Constructor
	 */
	public ResponseStream(){
		//Not finished yet
		mFinished = false;
	}

	public void setLocalMachine(){
		mIsLocal = true;
	}
	
	public boolean isLocal() {
		return mIsLocal;
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
		mFinalResponse = mJSON.toString().replaceAll ("\\\\/", "/");
		
		//It's finished
		mFinished = true;
	}
	
	public void hardEndStatus(String zResult) {
		//Hard set the response..
		mFinalResponse = zResult;
		
		//It's finished
		mFinished = true;
	}
	
	public boolean isFinished() {
		return mFinished;
	}
	
	/**
	 * Wait a maximum amount of time..
	 */
	public void waitToFinish() {
		long timestart = System.currentTimeMillis();
		long timediff  = 0;
		
		//5 second max wait..
		while(!mFinished && timediff<MAX_WAITTIME) {
			//Wait 20ms..
			try {Thread.sleep(20);} catch (InterruptedException e) {}
			
			//Calculate
			timediff = System.currentTimeMillis() - timestart;
		}
	}
}
