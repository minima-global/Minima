package org.minima.system.network.minidapps.minilib;

import org.minima.system.network.commands.FILE;
import org.minima.system.network.minidapps.minibackend.BackEndDAPP;
import org.minima.utils.json.JSONObject;
import org.mozilla.javascript.Function;
import org.mozilla.javascript.NativeObject;

public class JSFile {

	/**
	 * JS BACKEND link
	 */
	private BackEndDAPP mBackBone;
	
	public JSFile(BackEndDAPP zBackBone) {
		mBackBone = zBackBone;
	}
	
	/**
	 * File Access Functions
	 */
	public void save(Object zObject, String zFilename) {
		save(zObject, zFilename, null);	 
	}
	
	public void save(Object zObject, String zFilename, Function zCallback) {
		//Create a JSON
		JSONObject jobj = MiniLibUtility.toJsonObject((NativeObject)zObject);
		
		//Now create the command..
		String command = "save "+zFilename+" "+jobj.toString();
		
		//Convert to String command..
		FILE ffunc = new FILE(command, mBackBone.getMiniDAPPID(), zCallback, mBackBone.getContext(), mBackBone.getScope());
		
		//Run it..
		ffunc.run();
	}
	
	/**
	 * Load a JSON from a file and send to the callback function
	 * @param zFilename
	 * @param zCallback
	 */
	public void load(String zFilename, Function zCallback) {
		//Now create the command..
		String command = "load "+zFilename;
		
		//Convert to String command..
		FILE ffunc = new FILE(command, mBackBone.getMiniDAPPID(), zCallback, mBackBone.getContext(), mBackBone.getScope());
		
		//Run it..
		ffunc.run();
	}
	
	/**
	 * Delete a file..
	 * @param zFilename
	 */
	public void delete(String zFilename) {
		delete(zFilename, null);	
	}
	
	public void delete(String zFilename, Function zCallback) {
		//Now create the command..
		String command = "delete "+zFilename;
		
		//Convert to String command..
		FILE ffunc = new FILE(command, mBackBone.getMiniDAPPID(), zCallback, mBackBone.getContext(), mBackBone.getScope());
		
		//Run it..
		ffunc.run();
	}
	
	/**
	 * List the contents of a folder
	 * 
	 * @param zFolder
	 * @param zCallback
	 */
	public void list(String zFilename, Function zCallback) {
		//Now create the command..
		String command = "list "+zFilename;
		
		//Convert to String command..
		FILE ffunc = new FILE(command, mBackBone.getMiniDAPPID(), zCallback, mBackBone.getContext(), mBackBone.getScope());
		
		//Run it..
		ffunc.run();
	}
	
}
