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

public class MinimaJS {
	
	BackEndDAPP mBackBone;
	
	String mMiniDAPPID = "0x00";
	
	public MinimaJS(String zMiniDAPPID, BackEndDAPP zBackBone) {
		mMiniDAPPID  = zMiniDAPPID;
		mBackBone    = zBackBone;
	}
	
	/**
	 * Log data to Standard out
	 * @param zLog
	 */
	public void log(String zLog) {
		MinimaLogger.log("MinimaJS log - "+zLog);
	}
	
	/**
	 * Main Minima Command
	 * 
	 * @param zCommand
	 */
	public void cmd(String zCommand) {
		cmd(zCommand,null);
	}
	
	public void cmd(String zCommand, Function zCallback) {
		MinimaLogger.log("MinimaJS command - "+zCommand+" "+zCallback);
		
		//Create a Command 
		Command cmd = new Command(zCommand, zCallback, mBackBone.getContext(), mBackBone.getScope());
		
		//Run it..
		Thread cmdthread = new Thread(cmd);
		cmdthread.start();
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
		File dapp = new File(mini,mMiniDAPPID);
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
		}
		
		MinimaLogger.log("JS File Save "+text+" "+savefile.getAbsolutePath());
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
	
	public void delete(String zFilename, Function zCallback) {
		
	}
	
	/**
	 * Main SQL function
	 * 
	 * @param zCommand
	 */
	public void sql(String zCommand) {
		sql(zCommand, null);
	}
	
	public void sql(String zCommand, Function zCallback) {
		MinimaLogger.log("MinimaJS sql -"+zCommand);
	}
	
	/**
	 * Network Functions
	 */
	//Post a message to the from backend to frontend.. or vice versa
	public void post(Object zObject) {
		
	}
	
	public void send(String zMinDAPPID, Object zObject) {
		
	}
	

	
	
}
