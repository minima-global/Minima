package org.minima.system.network.commands;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.StringTokenizer;

import org.minima.objects.base.MiniData;
import org.minima.system.Main;
import org.minima.system.brains.BackupManager;
import org.minima.utils.MiniFile;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

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
		File minidappfolder  = backup.getMiniDAPPFolder(mMiniDAPPID);
		File tempfolder      = backup.getTempFolder();
		
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
				response.put("success", true);
				
			} catch (Exception e) {
				response.put("success", false);
				response.put("exception", e.toString());
				e.printStackTrace();
			}
			
		}else if(filefunc.equals("savehex")) {
			//Make sure the parent folder exists..
			thefile.getParentFile().mkdirs();
			
			//Get the file index
			int index        = mCommand.indexOf(file);
			String filedata  = mCommand.substring(index + file.length()).trim();
			
			if(!filedata.startsWith("0x")) {
				//ERROR
				response.put("success", false);
				response.put("exception", "Not HEX - must start with 0x..");
			}else {
				try {
					MiniData hex = new MiniData(filedata);
					MiniFile.writeDataToFile(thefile, hex.getData());
					response.put("success", true);
					
				} catch (Exception e) {
					response.put("success", false);
					response.put("exception", e.toString());
					e.printStackTrace();
				}	
			}
			
			
		}else if(filefunc.equals("load")) {
			if(thefile.exists()) {
				try {
					byte[] data = MiniFile.readCompleteFile(thefile);
					response.put("data", new String(data,Charset.forName("UTF-8")));
					response.put("success", true);
					
				} catch (IOException e) {
					response.put("success", false);
					response.put("exception", e.toString());
					e.printStackTrace();
				}
			}else {
				response.put("success", false);
				response.put("exception", "..does not exist!");
			}
		
		}else if(filefunc.equals("loadhex")) {
			if(thefile.exists()) {
				try {
					byte[] data = MiniFile.readCompleteFile(thefile);
					MiniData hex = new MiniData(data);
					
					response.put("data", hex.to0xString());
					response.put("success", true);
					
				} catch (IOException e) {
					response.put("success", false);
					response.put("exception", e.toString());
					e.printStackTrace();
				}
			}else {
				response.put("success", false);
				response.put("exception", "..does not exist!");
			}
		
		}else if(filefunc.equals("copy")) {
			String newfile = strtok.nextToken().trim();
			File copyto = new File(minidappfolder, newfile);
			
			//Do the move..
			try {
				MiniFile.copyFile(thefile, copyto);
				response.put("success", true);
				response.put("copy", newfile);
				
			} catch (IOException e) {
				response.put("success", false);
				response.put("exception", e.toString());
			}
			
		}else if(filefunc.equals("move")) {
			String newfile = strtok.nextToken().trim();
			File moveto = new File(minidappfolder, newfile);
			
			//Check parents exis
			File parent = moveto.getParentFile();
			parent.mkdirs();
					
			//Do the move..
			boolean success = thefile.renameTo(moveto);
			
			response.put("success", success);
			response.put("renamed", newfile);
			
		}else if(filefunc.equals("movetotemp")) {
			String tempfile = strtok.nextToken().trim();
			File moveto = new File(tempfolder, tempfile);
			
			//Check parents exis
			File parent = moveto.getParentFile();
			parent.mkdirs();
					
			//Do the move..
			boolean success = thefile.renameTo(moveto);
			
			String tempbasepath  = tempfolder.getAbsolutePath();
			String tempfinalpath = tempfile.substring(tempbasepath.length());
			
			response.put("success", success);
			response.put("renamed", tempbasepath);
			
		}else if(filefunc.equals("movefromtemp")) {
			String tempfile = strtok.nextToken().trim();
			File movefrom = new File(tempfolder, tempfile);
			
			//Check parents exist
			File parent = thefile.getParentFile();
			parent.mkdirs();
			
			if(!movefrom.exists()) {
				response.put("success", false);
				response.put("exception", "file does not exist");
			}else {
				//Do the move..
				boolean success = movefrom.renameTo(thefile);
				
				response.put("success", success);
				response.put("renamed", finalpath);
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
