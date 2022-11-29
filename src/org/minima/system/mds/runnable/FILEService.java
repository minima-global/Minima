package org.minima.system.mds.runnable;

import org.minima.system.mds.MDSManager;
import org.minima.system.mds.handler.FILEcommand;
import org.mozilla.javascript.Context;
import org.mozilla.javascript.Function;
import org.mozilla.javascript.NativeJSON;
import org.mozilla.javascript.Scriptable;

public class FILEService {

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
	
	public FILEService(MDSManager zMDS, String zMiniDAPPID, String zMiniDAPPName, Context zContext, Scriptable zScope) {
		mMDS			= zMDS;
		mMiniDAPPID		= zMiniDAPPID;
		mMiniDAPPName	= zMiniDAPPName;
		mContext 		= zContext;
		mScope 			= zScope;
	}
	
	/**
	 * LIST
	 */
	public void list(String zFile) {
		list(zFile,null);
	}
	
	public void list(String zFile, Function zCallback) {
		
		//Create a Command and run it..
		FILEcommand fc = new FILEcommand(mMDS, mMiniDAPPID, FILEcommand.FILECOMMAND_LIST, 
				zFile, "");
		String result = fc.runCommand();
		
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
	 * SAVE
	 */
	public void save(String zFile, String zData) {
		save(zFile,zData,null);
	}
	
	public void save(String zFile, String zData, Function zCallback) {
		
		//Create a Command and run it..
		FILEcommand fc = new FILEcommand(mMDS, mMiniDAPPID, FILEcommand.FILECOMMAND_SAVE, 
				zFile, zData);
		String result = fc.runCommand();
		
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
	 * LOAD
	 */
	public void load(String zFile) {
		load(zFile,null);
	}
	
	public void load(String zFile, Function zCallback) {
		
		//Create a Command and run it..
		FILEcommand fc = new FILEcommand(mMDS, mMiniDAPPID, FILEcommand.FILECOMMAND_LOAD, 
				zFile, "");
		String result = fc.runCommand();
		
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
	 * DELETE
	 */
	public void delete(String zFile) {
		delete(zFile,null);
	}
	
	public void delete(String zFile, Function zCallback) {
		
		//Create a Command and run it..
		FILEcommand fc = new FILEcommand(mMDS, mMiniDAPPID, FILEcommand.FILECOMMAND_DELETE, 
				zFile, "");
		String result = fc.runCommand();
		
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
