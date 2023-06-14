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
		FILEcommand fc = new FILEcommand(mMDS, mMiniDAPPID, 
				FILEcommand.FILECOMMAND_LIST, zFile, "");
		String result = fc.runCommand();
		
		//Run the callback
		runCallback(zCallback, result);
	}
	
	/**
	 * SAVE
	 */
	public void save(String zFile, String zData) {
		save(zFile,zData,null);
	}
	
	public void save(String zFile, String zData, Function zCallback) {
		
		//Create a Command and run it..
		FILEcommand fc = new FILEcommand(mMDS, mMiniDAPPID, 
				FILEcommand.FILECOMMAND_SAVE, zFile, zData);
		String result = fc.runCommand();
		
		//Run the callback
		runCallback(zCallback, result);
	}
	
	/**
	 * SAVE BINARY
	 */
	public void savebinary(String zFile, String zData) {
		savebinary(zFile,zData,null);
	}
	
	public void savebinary(String zFile, String zData, Function zCallback) {
		
		//Create a Command and run it..
		FILEcommand fc = new FILEcommand(mMDS, mMiniDAPPID, 
				FILEcommand.FILECOMMAND_SAVEBINARY, zFile, zData);
		String result = fc.runCommand();
		
		//Run the callback
		runCallback(zCallback, result);
	}
	
	/**
	 * LOAD
	 */
	public void load(String zFile) {
		load(zFile,null);
	}
	
	public void load(String zFile, Function zCallback) {
		
		//Create a Command and run it..
		FILEcommand fc = new FILEcommand(mMDS, mMiniDAPPID, 
				FILEcommand.FILECOMMAND_LOAD, zFile, "");
		String result = fc.runCommand();
		
		//Run the callback
		runCallback(zCallback, result);
	}
	
	/**
	 * LOAD BINARY
	 */
	public void loadbinary(String zFile) {
		loadbinary(zFile,null);
	}
	
	public void loadbinary(String zFile, Function zCallback) {
		
		//Create a Command and run it..
		FILEcommand fc = new FILEcommand(mMDS, mMiniDAPPID, 
				FILEcommand.FILECOMMAND_LOADBINARY, zFile, "");
		String result = fc.runCommand();
		
		//Run the callback
		runCallback(zCallback, result);
	}
	
	/**
	 * DELETE
	 */
	public void delete(String zFile) {
		delete(zFile,null);
	}
	
	public void delete(String zFile, Function zCallback) {
		
		//Create a Command and run it..
		FILEcommand fc = new FILEcommand(mMDS, mMiniDAPPID, 
				FILEcommand.FILECOMMAND_DELETE, zFile, "");
		String result = fc.runCommand();
		
		//Run the callback
		runCallback(zCallback, result);
	}
	
	/**
	 * GETPATH
	 */
	public void getpath(String zFile) {
		getpath(zFile,null);
	}
	
	public void getpath(String zFile, Function zCallback) {
		
		//Create a Command and run it..
		FILEcommand fc = new FILEcommand(mMDS, mMiniDAPPID, 
				FILEcommand.FILECOMMAND_GETPATH, zFile, "");
		String result = fc.runCommand();
		
		//Run the callback
		runCallback(zCallback, result);
	}
	
	/**
	 * MAKEDIR
	 */
	public void makedir(String zFile) {
		makedir(zFile,null);
	}
	
	public void makedir(String zFile, Function zCallback) {
		
		//Create a Command and run it..
		FILEcommand fc = new FILEcommand(mMDS, mMiniDAPPID, 
				FILEcommand.FILECOMMAND_MAKEDIR, zFile, "");
		String result = fc.runCommand();
		
		//Run the callback
		runCallback(zCallback, result);
	}
	
	/**
	 * COPY
	 */
	public void copy(String zFile, String zData) {
		copy(zFile,zData,null);
	}
	
	public void copy(String zFile, String zData, Function zCallback) {
		
		//Create a Command and run it..
		FILEcommand fc = new FILEcommand(mMDS, mMiniDAPPID, 
				FILEcommand.FILECOMMAND_COPY, zFile, zData);
		String result = fc.runCommand();
		
		//Run the callback
		runCallback(zCallback, result);
	}
	
	/**
	 * MOVE
	 */
	public void move(String zFile, String zData) {
		move(zFile,zData,null);
	}
	
	public void move(String zFile, String zData, Function zCallback) {
		
		//Create a Command and run it..
		FILEcommand fc = new FILEcommand(mMDS, mMiniDAPPID, 
				FILEcommand.FILECOMMAND_MOVE, zFile, zData);
		String result = fc.runCommand();
		
		//Run the callback
		runCallback(zCallback, result);
	}
	
	/**
	 * DOWNLOAD
	 */
	public void download(String zURL) {
		download(zURL,null);
	}
	
	public void download(String zURL, Function zCallback) {
		
		//Create a Command and run it..
		FILEcommand fc = new FILEcommand(mMDS, mMiniDAPPID, 
				FILEcommand.FILECOMMAND_DOWNLOAD, zURL, "");
		String result = fc.runCommand();
		
		//Run the callback
		runCallback(zCallback, result);
	}
	
	/**
	 * COPYTOWEB
	 */
	public void copytoweb(String zFile, String zCopy) {
		copytoweb(zFile, zCopy, null);
	}
	
	public void copytoweb(String zFile, String zCopy, Function zCallback) {
		
		//Create a Command and run it..
		FILEcommand fc = new FILEcommand(mMDS, mMiniDAPPID, 
				FILEcommand.FILECOMMAND_COPYTOWEB, zFile, zCopy);
		String result = fc.runCommand();
		
		//Run the callback
		runCallback(zCallback, result);
	}
	
	/**
	 * Used by all functions to send results back to JS
	 */
	private void runCallback(Function zCallback, String zResult) {
		//Send Info Back
		if(zCallback == null) {
			return;
		}
		
		//The arguments
		Object[] args = { NativeJSON.parse(mContext, mScope, zResult, new NullCallable()) };
		
		//Call the main MDS Function in JS
		zCallback.call(mContext, mScope, mScope, args);
	}
	
}