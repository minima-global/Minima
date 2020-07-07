package org.minima.system.network.minidapps.minilib;

import org.minima.system.network.commands.FILE;
import org.mozilla.javascript.Function;

public class JSFile {

	private FILE mFileAccess;
	
	public JSFile() {
		// TODO Auto-generated constructor stub
	}
	

	/**
	 * File Access Functions
	 */
	public void save(Object zObject, String zFilename) {
		save(zObject, zFilename, null);	 
	}
	
	public void save(Object zObject, String zFilename, Function zCallback) {

	}
	
	/**
	 * Load a JSON from a file and send to the callback function
	 * @param zFilename
	 * @param zCallback
	 */
	public void load(String zFilename, Function zCallback) {
	
	}
	
	/**
	 * Delete a file..
	 * @param zFilename
	 */
	public void delete(String zFilename) {
		delete(zFilename, null);	
	}
	
	public void delete(String zFilename, Function zCallback) {
		
	}
	
	/**
	 * List the contents of a folder
	 * 
	 * @param zFolder
	 * @param zCallback
	 */
	public void list(String zFolder, Function zCallback) {
		
	}
	
}
