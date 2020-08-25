package org.minima.system.network.commands;

import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.charset.Charset;
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
	
	//The Final Result..
	String mFinalResult = "";
		
	public FILE(String zCommand, String zMiniDAPPID) {
		mCommand    = zCommand;
		mMiniDAPPID = zMiniDAPPID;
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
		BackupManager backup = Main.getMainHandler().getBackupManager();
		File minidappfolder  = backup.getMiniDAPPFilesFolder(mMiniDAPPID);
		
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
			int index        = mCommand.indexOf(file);
			String filedata  = mCommand.substring(index + file.length()).trim();
			
			try {		
				MiniFile.writeDataToFile(thefile, filedata.getBytes(Charset.forName("UTF-8")));			
			} catch (Exception e) {
				response.put("exception", e.toString());
				e.printStackTrace();
			}
			
		}else if(filefunc.equals("move")) {
			String newfile = strtok.nextToken().trim();
			File moveto = new File(minidappfolder, newfile);
			
			//Check parents exis
			File parent = moveto.getParentFile();
			parent.mkdirs();
					
			//Do the move..
			boolean success = thefile.renameTo(moveto);
			
			response.put("renamed", newfile);
			response.put("move", success);
			
		}else if(filefunc.equals("load")) {
			if(thefile.exists()) {
				try {
					byte[] data = MiniFile.readCompleteFile(thefile);
					response.put("data", new String(data,Charset.forName("UTF-8")));
					
				} catch (IOException e) {
					response.put("exception", e.toString());
					e.printStackTrace();
				}
			}else {
				response.put("exception", "..does not exist!");
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
			
		}else if(filefunc.equals("delete")) {
			if(thefile.exists()) {
				//Delete
				BackupManager.safeDelete(thefile);
			}
		}
		
		//Convert to Text
		mFinalResult = response.toString();
	}
}
