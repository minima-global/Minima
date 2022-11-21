package org.minima.system.mds.handler;

import java.io.File;

import org.minima.objects.base.MiniString;
import org.minima.system.mds.MDSManager;
import org.minima.utils.MiniFile;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class FILEcommand {

	public static final String FILECOMMAND_LIST 	= "LIST";
	public static final String FILECOMMAND_SAVE 	= "SAVE";
	public static final String FILECOMMAND_LOAD 	= "LOAD";
	public static final String FILECOMMAND_DELETE 	= "DELETE";
	
	MDSManager mMDS;
	
	String mMiniDAPPID;
	
	String mFileCommand;
	
	String mFile;
	String mData;
	
	public FILEcommand(MDSManager zManager, String zMiniDAPPID,  String zCommand, String zFile, String zData) {
		mMDS			= zManager;
		mMiniDAPPID 	= zMiniDAPPID;
		mFileCommand	= zCommand;
		mFile			= zFile;
		mData			= zData;
	}
	
	public String runCommand() {
		
		//Default fail result
		JSONObject statfalse = new JSONObject();
		statfalse.put("command", mFileCommand);
		statfalse.put("file", mFile);
		statfalse.put("data", mData);
		statfalse.put("status", false);
		statfalse.put("pending", false);
		String result = statfalse.toJSONString();
		
		try {
		
			//Get the root folder..
			File rootfiles = mMDS.getMiniDAPPFileFolder(mMiniDAPPID);
			
			//Get the requested file..
			File actualfile = new File(rootfiles,mFile);
			
			//Check id child..
			if(!MiniFile.isChild(rootfiles, actualfile)) {
				throw new Exception("Invalid file..");
			}
			
			JSONObject resp = new JSONObject();
			resp.put("action", mFileCommand);
			resp.put("file", mFile);
			resp.put("data", mData);
			resp.put("exists", actualfile.exists());
			
			if(mFileCommand.equals(FILECOMMAND_LIST)) {
				
				//List the files..
				File[] files = actualfile.listFiles();
				if(files == null) {
					files = new File[0];
				}
				
				JSONArray listfiles = new JSONArray();
				for(File ff : files) {
					JSONObject fdata = new JSONObject();
					fdata.put("name", ff.getName());
					fdata.put("size", ff.length());
					fdata.put("isdir", ff.isDirectory());
					fdata.put("isfile", ff.isFile());
					listfiles.add(fdata);
				}
			
				resp.put("list", listfiles);
			
			}else if(mFileCommand.equals(FILECOMMAND_SAVE)) {
				
				File parent = actualfile.getParentFile();
				if(!parent.exists()) {
					parent.mkdirs();
				}
				
				//Now Write data..
				MiniFile.writeDataToFile(actualfile, mData.getBytes(MiniString.MINIMA_CHARSET));
				
				JSONObject fdata = new JSONObject();
				fdata.put("name", actualfile.getName());
				fdata.put("size", actualfile.length());
				
				resp.put("save", fdata);
			
			}else if(mFileCommand.equals(FILECOMMAND_LOAD)) {
			
				byte[] data = MiniFile.readCompleteFile(actualfile);
				
				JSONObject fdata = new JSONObject();
				fdata.put("name", actualfile.getName());
				fdata.put("size", data.length);
				fdata.put("data", new String(data, MiniString.MINIMA_CHARSET));
				
				resp.put("load", fdata);
			
			}else if(mFileCommand.equals(FILECOMMAND_DELETE)) {
				
				JSONObject fdata = new JSONObject();
				fdata.put("name", actualfile.getName());
				
				actualfile.delete();
				
				resp.put("delete", fdata);
			}
			
			JSONObject stattrue = new JSONObject();
			stattrue.put("command", "file");
			stattrue.put("status", true);
			stattrue.put("pending", false);
			stattrue.put("response", resp);
			result = stattrue.toJSONString();
			
		}catch(Exception exc) {
			MinimaLogger.log("FILE command : "+mMiniDAPPID+" "+exc);
			
			statfalse.put("error", exc.toString());
			result = statfalse.toJSONString();
		}
		
		return result;
	}
	

}
