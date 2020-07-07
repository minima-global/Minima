package org.minima.system.network.commands;

import java.io.File;
import java.io.IOException;

import org.minima.objects.base.MiniString;
import org.minima.system.backup.BackupManager;
import org.minima.system.input.InputHandler;
import org.minima.system.network.minidapps.minilib.BackEndDAPP;
import org.minima.system.network.minidapps.minilib.JSMiniLibUtil;
import org.minima.utils.FileUtil;
import org.minima.utils.MiniFile;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.mozilla.javascript.Context;
import org.mozilla.javascript.Function;
import org.mozilla.javascript.NativeArray;
import org.mozilla.javascript.NativeObject;
import org.mozilla.javascript.Scriptable;

public class FILE {

	private String mMiniDAPPID;
	private BackEndDAPP mBackBone;
	
	//The Final Result..
	String mFinalResult = "";
		
	public FILE(BackEndDAPP zBackBone) {
		this(zBackBone, zBackBone.getMiniDAPPID());
	}
	
	public FILE(String zMiniDAPPID) {
		this(null,zMiniDAPPID);
	}
	
	public FILE(BackEndDAPP zBackBone, String zMiniDAPPID) {
		mBackBone   = zBackBone;
		mMiniDAPPID = zMiniDAPPID;
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
			JSONObject json = JSMiniLibUtil.toJsonObject(nativeObject);
			text = json.toString();
		
		}else if(zObject instanceof NativeArray) {
			NativeArray nativeObject = (NativeArray)zObject;
			JSONArray json = JSMiniLibUtil.toJsonArray(nativeObject);
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
			return;
		}
		
		//Now send the result back vis the callback..
		if(zCallback != null && mBackBone != null) {
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
	public void load(String zFilename, Function zCallback) {
		//Get the Backup mnager
		BackupManager back = InputHandler.getMainInputHandler().getMainHandler().getBackupManager();
		
		//get the MinDAPP Folder..
		File mini     = back.getMiniDAPPFolder();
		File dapp     = new File(mini,mMiniDAPPID);
		File fdir     = new File(dapp,"files");
		File savefile = new File(fdir,zFilename);
		
		//The response
		JSONObject response = new JSONObject();
		response.put("file", zFilename);
		
		if(!savefile.exists()) {
			response.put("exists", false);
		}else {
			response.put("exists", true);
			
			//Load the data..
			byte[] data;
			try {
				data = MiniFile.readCompleteFile(savefile);
			
				//Convert it to a MiniString
				MiniString json = new MiniString(data);
				
				//Add it to the JSON
				response.put("data", json.toString());
				
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				
				//Something went wrong
				response.put("exists", false);
				response.put("exception",e.toString());
			}
		}
	
		//Final text
		String ftext = response.toString(); 
		
		//Get the JS components
		Context context  = mBackBone.getContext();
		Scriptable scope = mBackBone.getScope();
		
		//Create a native JSON
		Object json = JSMiniLibUtil.makeJSONObject(ftext, context, scope);
		
		//Make a function variable list
		Object functionArgs[] = { json };
	    
		//Call the function..
		zCallback.call(context, scope, scope, functionArgs);
	}
	
	/**
	 * Delete a file..
	 * @param zFilename
	 */
	public void delete(String zFilename) {
		delete(zFilename,null);
	}
	
	public void delete(String zFilename, Function zCallback) {
		//Get the Backup mnager
		BackupManager back = InputHandler.getMainInputHandler().getMainHandler().getBackupManager();
		
		//get the MinDAPP Folder..
		File mini     = back.getMiniDAPPFolder();
		File dapp     = new File(mini,mMiniDAPPID);
		File fdir     = new File(dapp,"files");
		File savefile = new File(fdir,zFilename);
		
		savefile.delete();
		
		//Now send the result back vis the callback..
		if(zCallback != null && mBackBone != null) {
			//Make a function variable list
			Object functionArgs[] = { };
		    
			//Call the function..
			zCallback.call(mBackBone.getContext(), mBackBone.getScope(), mBackBone.getScope(), functionArgs);
		}
	}

	
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
		File dapp = new File(mini,mMiniDAPPID);
		File fdir = new File(dapp,"files");
		fdir.mkdirs();
		
		//Now add the zFolder..
		File checker = new File(fdir,zFolder);
		File[] files = checker.listFiles();
		
		//Create an Array of file objects
		JSONArray farr = new JSONArray();
		for(File ff : files) {
			JSONObject file = new JSONObject();
			file.put("name", ff.getName());
			file.put("dir", ff.isDirectory());
			if(!ff.isDirectory()) {
				file.put("size", ff.length());	
			}else {
				file.put("size", 0);
			}
			farr.add(file);
		}
		
		//Create the Response JSON
		JSONObject response = new JSONObject();
		response.put("folder", zFolder);
		response.put("files",farr);
		
		//Final text
		String ftext = response.toString(); 
		
		//Get the JS components
		Context context  = mBackBone.getContext();
		Scriptable scope = mBackBone.getScope();
		
		//Create a native JSON
		Object json = JSMiniLibUtil.makeJSONObject(ftext, context, scope);
		
		//Make a function variable list
		Object functionArgs[] = { json };
	    
		//Call the function..
		zCallback.call(context, scope, scope, functionArgs);
	}
}
