package org.minima.system.mds.runnable.api;

import org.minima.database.minidapps.MiniDAPP;
import org.minima.objects.base.MiniData;
import org.minima.system.mds.MDSManager;
import org.minima.system.mds.handler.APIAutoResponse;
import org.minima.system.mds.handler.APICommand;
import org.minima.system.mds.runnable.NullCallable;
import org.minima.utils.MinimaLogger;
import org.mozilla.javascript.Context;
import org.mozilla.javascript.Function;
import org.mozilla.javascript.NativeJSON;
import org.mozilla.javascript.Scriptable;

public class APIService {

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
	
	public APIService(MDSManager zMDS, String zMiniDAPPID, String zMiniDAPPName, Context zContext, Scriptable zScope) {
		mMDS			= zMDS;
		mMiniDAPPID		= zMiniDAPPID;
		mMiniDAPPName	= zMiniDAPPName;
		mContext 		= zContext;
		mScope 			= zScope;
	}
	
	/**
	 * Call an API on a different MiniDAPP
	 */
	public void call(String zDappName, String zData) {
		call(zDappName, zData, null);
	}
	
	public void call(String zDappName, String zData, Function zCallback) {
		
		//Come up with a random ID
		String rand = MiniData.getRandomData(16).to0xString();
		
		//Create a new Call Object
		mMDS.addAPICall(new APICallback(mContext, mScope, rand, zCallback));
				
		//Get the MiniDAPP we are trying to talk to
		MiniDAPP md = mMDS.getMiniDAPPFromName(zDappName);
		
		//Is it a Valid MiniDAPP 
		if(md == null) {
			
			//Create a Timed auto response..
			APIAutoResponse auto = new APIAutoResponse(mMDS, zDappName, mMiniDAPPName, mMiniDAPPID,  rand);
			auto.setImmediate();
			auto.runauto();
			
			return;
		}
		
		//Create a Command and run it..
		APICommand comms = new APICommand(mMDS, mMiniDAPPName, zDappName, md.getUID(), zData, rand, true);
		String result 	 = comms.runCommand();
		
		//Create a Timed auto response..
		APIAutoResponse auto = new APIAutoResponse(mMDS, zDappName, mMiniDAPPName, mMiniDAPPID,  rand);
		auto.runauto();
		
		//Don't return anything to the callback - that is done later..
	}
	
	/**
	 * Send a PRIVATE message
	 */
	public void reply(String zToDapp, String zRandID, String zData) {
		reply(zToDapp, zRandID, zData, null);
	}
	
	public void reply(String zDappName, String zRandID, String zData, Function zCallback) {
		
		//Get the MiniDAPP we are trying to talk to
		MiniDAPP md = mMDS.getMiniDAPPFromName(zDappName);
			
		//Construct command
		APICommand comms = new APICommand(mMDS, mMiniDAPPName, zDappName, md.getUID(), zData, zRandID, false);
		String result = comms.runCommand();
		
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