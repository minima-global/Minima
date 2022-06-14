package org.minima.system.mds.runnable;

import org.minima.system.commands.Command;
import org.minima.system.mds.MDSManager;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.mozilla.javascript.Callable;
import org.mozilla.javascript.Context;
import org.mozilla.javascript.Function;
import org.mozilla.javascript.NativeJSON;
import org.mozilla.javascript.Scriptable;

public class MDSJS {

	/**
	 * Required to create the Native JSON
	 */
	private class NullCallable implements Callable{
	    @Override
	    public Object call(Context context, Scriptable scope, Scriptable holdable, Object[] objects){
	        return objects[1];
	    }
	}
	
	String mMiniDAPPID;
	Context mContext;
	Scriptable 	mScope;
	Function mMainCallback;
	MDSManager mMDS;
	
	public MDSJS(MDSManager zMDS, String zMiniDAPPID, Context zContext, Scriptable zScope) {
		mMDS		= zMDS;
		mMiniDAPPID	= zMiniDAPPID;
		mContext 	= zContext;
		mScope 		= zScope;
	}
	
	public void shutdown() {
		mContext.exit();
	}
	
	/**
	 * Main Callback for Minima events
	 */
	public void callMainCallback(JSONObject zEvent) {

		//Forward the message as a Native JS JSONObject
		if(mMainCallback != null) {
			//The argumnets
			Object[] args = { NativeJSON.parse(mContext, mScope, zEvent.toString(), new NullCallable()) };
			
			//Call the main MDS Function in JS
			mMainCallback.call(mContext, mScope, mScope, args);
		}
	}
	
	/**
	 * Simple Log
	 */
	public void log(String zMessage) {
		MinimaLogger.log("MDS_"+mMiniDAPPID+"> "+zMessage);
	}
	
	/**
	 * Init Call
	 */
	public void init(Function zCallback) {
		
		//Store this for later
		mMainCallback = zCallback;
		
		//Create the init message
		JSONObject init = new JSONObject();
		init.put("event", "inited");
	
		//Send to the Runnable
		callMainCallback(init);
	}
	
	/**
	 * The Main CMD function
	 */
	public void cmd(String zCommand) {
		cmd(zCommand, null);
	}
	
	public void cmd(String zCommand, Function zCallback) {
	
		//Run the command
		JSONArray res = Command.runMultiCommand(zCommand);
    	
    	//Get the result.. is it a multi command or single.. 
		String result = null;
		if(res.size() == 1) {
			result = res.get(0).toString();
		}else {
			result = res.toString();
		}
		
		if(zCallback == null) {
			return;
		}
		
		//The argumnets
		Object[] args = { NativeJSON.parse(mContext, mScope, result.toString(), new NullCallable()) };
		
		//Call the main MDS Function in JS
		zCallback.call(mContext, mScope, mScope, args);
	}
	
	/**
	 * SQL Function
	 */
	public void sql(String zCommand) {
		sql(zCommand, null);
	}
	
	public void sql(String zSQL, Function zCallback) {
		
		//Run the SQL
		JSONObject sqlresult = mMDS.runSQL(mMiniDAPPID, zSQL);
		
		if(zCallback == null) {
			return;
		}
		
		//The argumnets
		Object[] args = { NativeJSON.parse(mContext, mScope, sqlresult.toString(), new NullCallable()) };
		
		//Call the main MDS Function in JS
		zCallback.call(mContext, mScope, mScope, args);
	}
}
