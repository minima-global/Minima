package org.minima.system.mds.runnable;

import org.minima.system.mds.MDSManager;
import org.minima.system.mds.handler.CMDcommand;
import org.minima.system.mds.handler.NOTIFYcommand;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.mozilla.javascript.Context;
import org.mozilla.javascript.Function;
import org.mozilla.javascript.NativeJSON;
import org.mozilla.javascript.Scriptable;

public class MDSJS {
	
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
	
	/**
	 * Maxin MInima callback
	 */
	Function mMainCallback;
	
	/**
	 * Maxin MDS manager - for SQL calls
	 */
	MDSManager mMDS;
	
	/**
	 * The NET Functions
	 */
	public NETService net;
	
	/**
	 * The COMMS service 
	 */
	public COMMSService comms;
	
	/**
	 * The FILE service
	 */
	public FILEService file;
	
	/**
	 * Main Constructor
	 * 
	 * @param zMDS
	 * @param zMiniDAPPID
	 * @param zMiniName
	 * @param zContext
	 * @param zScope
	 */
	public MDSJS(MDSManager zMDS, String zMiniDAPPID, String zMiniName,  Context zContext, Scriptable zScope) {
		mMDS			= zMDS;
		mMiniDAPPID		= zMiniDAPPID;
		mMiniDAPPName	= zMiniName;
		mContext 		= zContext;
		mScope 			= zScope;
		net 			= new NETService(zMiniDAPPID, zMiniName, zContext, zScope);
		comms			= new COMMSService(mMDS, zMiniDAPPID, zMiniName, zContext, zScope);
		file			= new FILEService(mMDS, zMiniDAPPID, zMiniName, zContext, zScope);
	}
	
	public String getMiniDAPPID() {
		return mMiniDAPPID;
	}
	
	public void shutdown() {
		//And now shut it down..
		mContext.exit();
	}
	
	/**
	 * Main Callback for Minima events
	 */
	public void callMainCallback(JSONObject zEvent) {

		//Forward the message as a Native JS JSONObject
		if(mMainCallback != null) {
			
			//Call the main MDS Function in JS
			mMainCallback.call(mContext, mScope, mScope, makeNativeJSONArgs(zEvent));
		}
	}
	
	/**
	 * Simple Log
	 */
	public void log(String zMessage) {
		MinimaLogger.log("MDS_"+mMiniDAPPName+"_"+mMiniDAPPID+" > "+zMessage, false);
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
		
		/**
		 * FOR NOW -ADD A SCEOND CALL!
		 */
		//Create the init message
		JSONObject initnew = new JSONObject();
		initnew.put("event", "MDSINIT");
	
		//Send to the Runnable
		callMainCallback(initnew);
	}
	
	/**
	 * Send a SHUTDOWN message
	 */
	public void sendshutdown() {
		//Create the init message
		JSONObject shutd = new JSONObject();
		shutd.put("event", "MDSSHUTDOWN");
	
		//Send to the Runnable
		callMainCallback(shutd);
	}
	
	/**
	 * The Main CMD function
	 */
	public void cmd(String zCommand) {
		cmd(zCommand, null);
	}
	
	public void cmd(String zCommand, Function zCallback) {
	
		//Create a Command
		CMDcommand cmd = new CMDcommand(mMiniDAPPID, zCommand);
		
		//Run it
		String result  = cmd.runCommand();
		
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
		
		//Call the main MDS Function in JS
		zCallback.call(mContext, mScope, mScope, makeNativeJSONArgs(sqlresult));
	}
	
	/**
	 * Notify Function
	 */
	public void notify(String zText) {
			
		//Create a Command
		NOTIFYcommand notify = new NOTIFYcommand(mMiniDAPPID, mMiniDAPPName, zText, true);
		notify.runCommand();
	}
	
	public void notifycancel() {
		
		//Create a Command
		NOTIFYcommand notify = new NOTIFYcommand(mMiniDAPPID, "", "", false);
		notify.runCommand();
	}
	
	/**
	 * Helper to create a JS JSON
	 */
	public Object[] makeNativeJSONArgs(JSONObject zJSON) {
		
		//Create the Native JSON Object
		Object[] args =  { NativeJSON.parse(mContext, mScope, zJSON.toString(), new NullCallable()) };
		
		return args;
	}
}
