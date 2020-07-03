package org.minima.system.network.minidapps.minilib;

import java.io.File;
import java.io.IOException;

import org.minima.objects.base.MiniString;
import org.minima.system.backup.BackupManager;
import org.minima.system.input.InputHandler;
import org.minima.utils.MiniFile;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.mozilla.javascript.Function;
import org.mozilla.javascript.NativeArray;
import org.mozilla.javascript.NativeObject;

public class JSFile {

	private BackEndDAPP mBackBone;
	
	public JSFile(BackEndDAPP zBackBone) {
		mBackBone   = zBackBone;
	}
	
	/**
	 * File Access Functions
	 */
	public void save(Object zObject, String zFilename) {
		save(zObject, zFilename,null); 
	}
	
	public void save(Object zObject, String zFilename, Function zCallback) {
		String text = "";
		if(zObject instanceof NativeObject) {
			NativeObject nativeObject = (NativeObject)zObject;
			JSONObject json = JSUtil.toJsonObject(nativeObject);
			text = json.toString();
		
		}else if(zObject instanceof NativeArray) {
			NativeArray nativeObject = (NativeArray)zObject;
			JSONArray json = JSUtil.toJsonArray(nativeObject);
			text = json.toString();
		}
		
		//Now store it..
		MiniString ms = new MiniString(text);
		
		//Get the Backup mnager
		BackupManager back = InputHandler.getMainInputHandler().getMainHandler().getBackupManager();
		
		//get the MinDAPP Folder..
		File mini = back.getMiniDAPPFolder();
		File dapp = new File(mini,mBackBone.getMiniDAPPID());
		File fdir = new File(dapp,"files");
		fdir.mkdirs();
		
		//Now create the file..
		File savefile = new File(fdir,zFilename);
		
		//And store..
		try {
			MiniFile.writeObjectToFile(savefile, ms);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return;
		}
		
		//Now send the result back vis the callback..
		if(zCallback != null) {
			//Make a function variable list
			Object functionArgs[] = { };
		    
			//Call the function..
			zCallback.call(mBackBone.getContext(), mBackBone.getScope(), mBackBone.getScope(), functionArgs);
		}
	}
	
	/**
	 * Load a JSON from a file and send to the callback function
	 * @param zFilename
	 * @param zCallback
	 */
	public void load(String zFilename, Function zCallback) {}
	
	/**
	 * Delete a file..
	 * @param zFilename
	 */
	public void delete(String zFilename) {
		delete(zFilename,null);
	}
	
	public void delete(String zFilename, Function zCallback) {}

	
	/**
	 * List the contents of a folder
	 * 
	 * @param zFolder
	 * @param zCallback
	 */
	public void list(String zFolder, Function zCallback) {
		//Get the Backup mnager
		BackupManager back = InputHandler.getMainInputHandler().getMainHandler().getBackupManager();
		
		//get the MinDAPP Folder..
		File mini = back.getMiniDAPPFolder();
		File dapp = new File(mini,mBackBone.getMiniDAPPID());
		File fdir = new File(dapp,"files");
		fdir.mkdirs();
		
		//Now add the zFolder..
		File checker = new File(fdir,zFolder);
		File[] files = checker.listFiles();
		
		
		
	}
}
