package org.minima.system.network.commands;

import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.StringTokenizer;

import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.brains.BackupManager;
import org.minima.system.input.InputHandler;
import org.minima.system.network.minidapps.minibackend.MiniJSONUtility;
import org.minima.utils.MiniFile;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.mozilla.javascript.Context;
import org.mozilla.javascript.Function;
import org.mozilla.javascript.Scriptable;

public class FILE implements Runnable {

	//The Command to run
	String mCommand;
	String mMiniDAPPID;
	
	//Call back with the response when finished
	Function   mCallback;
	Context    mContext;
	Scriptable mScope;
	
	//The Final Result..
	String mFinalResult = "";
		
	public FILE(String zCommand, String zMiniDAPPID) {
		this(zCommand, zMiniDAPPID, null,null,null);
	}
	
	public FILE(String zCommand, String zMiniDAPPID, Function zCallback, Context zContext, Scriptable zScope) {
		mCommand    = zCommand;
		mMiniDAPPID = zMiniDAPPID;
		mCallback   = zCallback;
		mContext    = zContext;
		mScope      = zScope;
	}

	public String getFinalResult() {
		return mFinalResult;
	}

	@Override
	public void run() {
		//Details
		StringTokenizer strtok = new StringTokenizer(mCommand," ");
		String filefunc = strtok.nextToken().trim();
		String file     = strtok.nextToken().trim();
		
		//Where is the database..
		File minidappfolder  = null;
		BackupManager backup = Main.getMainHandler().getBackupManager();
		
		//Which Database.. could be running from a folder..
		if(mMiniDAPPID.length()<16) {
			//Get the database folder
			File temp = BackupManager.getTempFolder();
			minidappfolder = new File(temp,"_files"+mMiniDAPPID);
			
		}else {
			//Get the database folder
			File minidapps   = backup.getMiniDAPPFolder();
			File dapp        = new File(minidapps,mMiniDAPPID);
			minidappfolder       = new File(dapp,"files");
		}
		
		//Make sure exists
		minidappfolder.mkdirs();
		
		//get the file
		File thefile = new File(minidappfolder,file);
		
		//Calculate the correct path
		String filepath  = thefile.getAbsolutePath();
		String basepath  = minidappfolder.getAbsolutePath();
		String finalpath = filepath.substring(basepath.length());
		
		//The response
		JSONObject response = new JSONObject();
		response.put("function", filefunc);
		response.put("file", finalpath);
		response.put("name", thefile.getName());
		response.put("exists", thefile.exists());
		
		//Which command is it..
		if(filefunc.equals("save")) {
			//Make sure the parent folder exists..
			thefile.getParentFile().mkdirs();
			
			//Get the file index
			int index    = mCommand.indexOf(file);
			String json  = mCommand.substring(index + file.length()).trim();
			
			try {		
				//Convert to a proper JSON Object..
				MiniString msdata = new MiniString(json);

				//Store to  file..	
				MiniFile.writeObjectToFile(thefile, msdata);
				
			} catch (Exception e) {
				response.put("exception", e.toString());
				e.printStackTrace();
			}
			
			//Convert to Text
			mFinalResult = response.toString();
			
		}else if(filefunc.equals("load")) {
			if(thefile.exists()) {
				//Load the data..
				try {
					FileInputStream fis = new FileInputStream(thefile);
					DataInputStream dis = new DataInputStream(fis);
					
					MiniString json = MiniString.ReadFromStream(dis);
				
					mFinalResult = json.toString();
					
					dis.close();
					fis.close();
				} catch (IOException e) {
					mFinalResult = "{}";
					e.printStackTrace();
				}
			}else {
				mFinalResult = "{}";
			}
			
		}else if(filefunc.equals("list")) {
			//Create an Array of file objects
			JSONArray farr = new JSONArray();
			
			if(thefile.exists()) {
				//Now add the zFolder..
				File[] files = thefile.listFiles();
				if(files == null) {
					files = new File[0];
				}
				
				if(thefile.isFile()) {
					response.put("directory", false);
					response.put("size", thefile.length());
				}else if(thefile.isDirectory()) {
					response.put("directory", true);
					response.put("size", files.length);
					
					for(File ff : files) {
						JSONObject filedesc = new JSONObject();
						filedesc.put("name", ff.getName());
						filedesc.put("dir", ff.isDirectory());
						if(!ff.isDirectory()) {
							filedesc.put("size", ff.length());	
						}else {
							filedesc.put("size", 0);
						}
						farr.add(filedesc);
					}
				}		
			}
			
			//Create the Response JSON
			response.put("files",farr);
			
			//Convert to Text
			mFinalResult = response.toString();
			
		}else if(filefunc.equals("delete")) {
			if(thefile.exists()) {
				//Delete
				BackupManager.safeDelete(thefile);
			}
			
			//Convert to Text
			mFinalResult = response.toString();
		}
		
		//Call the JS function
		if(mCallback != null) {		
			//Create a native JSON
			Object json = MiniJSONUtility.makeJSONObject(mFinalResult, mContext, mScope);
			
			//Make a function variable list
			Object functionArgs[] = { json };
		    
			//Call the function..
			mCallback.call(mContext, mScope, mScope, functionArgs);
		}
	}
}
