package org.minima.system.mds.handler;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.system.mds.MDSManager;
import org.minima.utils.MiniFile;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class FILEcommand {

	/**
	 * Base FILE Functions
	 */
	public static final String FILECOMMAND_LIST 			= "LIST";
	public static final String FILECOMMAND_SAVE 			= "SAVE";
	public static final String FILECOMMAND_LOAD 			= "LOAD";
	public static final String FILECOMMAND_DELETE 			= "DELETE";
	public static final String FILECOMMAND_SAVEBINARY 		= "SAVE_BINARY";
	public static final String FILECOMMAND_LOADBINARY 		= "LOAD_BINARY";
	public static final String FILECOMMAND_GETPATH 			= "GETPATH";
	public static final String FILECOMMAND_MAKEDIR 			= "MAKEDIR";
	public static final String FILECOMMAND_COPY 			= "COPY";
	public static final String FILECOMMAND_MOVE 			= "MOVE";
	public static final String FILECOMMAND_DOWNLOAD 		= "DOWNLOAD";
	public static final String FILECOMMAND_COPYTOWEB 		= "COPYTOWEB";
	public static final String FILECOMMAND_DELETEFROMWEB 	= "DELETEFROMWEB";
	public static final String FILECOMMAND_LISTWEB 			= "LISTWEB";
	
	/**
	 * Main MDS Manager
	 */
	MDSManager mMDS;
	
	/**
	 * Which MiniDAPP os this
	 */
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
		
			//The root folder
			File rootfiles		= mMDS.getMiniDAPPFileFolder(mMiniDAPPID);
			
			String canonical 	= "";
			File actualfile		= null;
			boolean fileexists 	= false;
			
			//Only do this for local files
			if(!mFileCommand.equals(FILECOMMAND_DOWNLOAD)) {
				
				//Get the requested file..
				actualfile = new File(rootfiles,mFile);
				fileexists = actualfile.exists();
				
				//Check id child..
				if(!MiniFile.isChild(rootfiles, actualfile)) {
					throw new Exception("Invalid file..");
				}
				
				canonical = getCanonicalPath(rootfiles,actualfile);
			}
			
			JSONObject resp = new JSONObject();
			resp.put("action", mFileCommand);
			resp.put("file", mFile);
			resp.put("canonical", canonical);
			resp.put("data", mData);
			resp.put("exists", fileexists);
			
			if(mFileCommand.equals(FILECOMMAND_LIST)) {
				
				boolean exists = actualfile.exists();
				JSONArray listfiles = new JSONArray();
				
				//List the files..
				if(actualfile.isDirectory() && exists) {
					File[] files = actualfile.listFiles();
					if(files == null) {
						files = new File[0];
					}
					
					//Sort the list
					Arrays.sort(files);
					for(File ff : files) {
						JSONObject fdata = new JSONObject();
						fdata.put("name", ff.getName());
						fdata.put("location", getCanonicalPath(rootfiles,ff));
						fdata.put("size", ff.length());
						fdata.put("modified", ff.lastModified());
						fdata.put("isdir", ff.isDirectory());
						fdata.put("isfile", ff.isFile());
						listfiles.add(fdata);
					}
				}
			
				resp.put("exists", exists);
				resp.put("isdir", actualfile.isDirectory());
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
				
			}else if(mFileCommand.equals(FILECOMMAND_SAVEBINARY)) {
				
				File parent = actualfile.getParentFile();
				if(!parent.exists()) {
					parent.mkdirs();
				}
			
				//Create the Binary data
				MiniData bindata = new MiniData(mData);
				
				//Now Write data..
				MiniFile.writeDataToFile(actualfile, bindata.getBytes());
				
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
			
			}else if(mFileCommand.equals(FILECOMMAND_LOADBINARY)) {
				
				byte[] data = MiniFile.readCompleteFile(actualfile);
				
				//Create the Binary data
				MiniData bindata = new MiniData(data);
				
				JSONObject fdata = new JSONObject();
				fdata.put("name", actualfile.getName());
				fdata.put("size", data.length);
				fdata.put("data", bindata.to0xString());
				
				resp.put("load", fdata);
			
			}else if(mFileCommand.equals(FILECOMMAND_DELETE)) {
				
				JSONObject fdata = new JSONObject();
				fdata.put("name", actualfile.getName());
				
				MiniFile.deleteFileOrFolder(rootfiles.getAbsolutePath(), actualfile);
				//actualfile.delete();
				
				resp.put("delete", fdata);

			}else if(mFileCommand.equals(FILECOMMAND_GETPATH)) {
				
				JSONObject fdata = new JSONObject();
				fdata.put("name", actualfile.getName());
				fdata.put("path", actualfile.getAbsolutePath());
				
				resp.put("getpath", fdata);
			
			}else if(mFileCommand.equals(FILECOMMAND_MAKEDIR)) {
				
				//Make this Directory
				actualfile.mkdirs();
				
				JSONObject fdata = new JSONObject();
				fdata.put("name", actualfile.getName());
				
				resp.put("makedir", fdata);
				
			}else if(mFileCommand.equals(FILECOMMAND_COPY)) {
				
				//The new file..
				File newfile = new File(rootfiles,mData);
				
				//Check id child..
				if(!MiniFile.isChild(rootfiles, newfile)) {
					throw new Exception("Invalid file..");
				}
				
				//Check exists
				if(!actualfile.exists()) {
					throw new IllegalArgumentException("File does not exist "+actualfile);
				}
				
				//Check not directory
				//if(actualfile.isDirectory()) {
				//	throw new IllegalArgumentException("Cannot copy Directory");
				//}
				
				File parent = newfile.getParentFile();
				if(!parent.exists()) {
					parent.mkdirs();
				}
				
				//Now copy the data
				MiniFile.copyFileOrFolder(actualfile, newfile);
				
				JSONObject fdata = new JSONObject();
				fdata.put("origfile", mFile);
				fdata.put("copyfile", mData);
				
				resp.put("copy", fdata);
			
			}else if(mFileCommand.equals(FILECOMMAND_MOVE)) {
				
				//The new file..
				File newfile = new File(rootfiles,mData);
				
				//Check id child..
				if(!MiniFile.isChild(rootfiles, newfile)) {
					throw new Exception("Invalid file..");
				}
				
				//Check exists
				if(!actualfile.exists()) {
					throw new IllegalArgumentException("File does not exist "+actualfile);
				}
				
				//Check not directory
				if(actualfile.isDirectory()) {
					throw new IllegalArgumentException("Cannot move Directory");
				}
				
				File parent = newfile.getParentFile();
				if(!parent.exists()) {
					parent.mkdirs();
				}
				
				//Now move the file
				actualfile.renameTo(newfile);
				
				JSONObject fdata = new JSONObject();
				fdata.put("origfile", mFile);
				fdata.put("movefile", mData);
				
				resp.put("move", fdata);
				
			}else if(mFileCommand.equals(FILECOMMAND_DOWNLOAD)) {
				
				//Make sure downloads folder exists
				File downs = new File(rootfiles,"Downloads");
				downs.mkdirs();
				
				//Get the filename
				String filename = Paths.get(new URI(mFile).getPath()).getFileName().toString();
				
				//What is the file
				File dfile = new File(downs,filename);
				if(dfile.exists()) {
					dfile.delete();
				}
				
				//Download the file to downloads folder
				long size = download(mFile, dfile.getPath());
				
				JSONObject fdata = new JSONObject();
				fdata.put("file", "/Downloads/"+filename);
				fdata.put("location", getCanonicalPath(rootfiles,dfile));
				fdata.put("path", dfile.getPath());
				fdata.put("size", size);
				resp.put("download", fdata);
			
			}else if(mFileCommand.equals(FILECOMMAND_COPYTOWEB)) {
				
				//Get the web Folder
				File webfiles = mMDS.getMiniDAPPWebFolder(mMiniDAPPID);
				
				//The new file..
				File newfile = new File(webfiles,mData);
				
				//Check id child..
				if(!MiniFile.isChild(webfiles, newfile)) {
					throw new Exception("Invalid file..");
				}
				
				//Check exists
				if(!actualfile.exists()) {
					throw new IllegalArgumentException("File does not exist "+actualfile);
				}
				
				//Check not directory
				//if(actualfile.isDirectory()) {
				//	throw new IllegalArgumentException("Cannot move Directory");
				//}
				
				File parent = newfile.getParentFile();
				if(!parent.exists()) {
					parent.mkdirs();
				}
				
				//Now copy the data
				MiniFile.copyFileOrFolder(actualfile, newfile);
				
				JSONObject fdata = new JSONObject();
				fdata.put("origfile", mFile);
				fdata.put("webfile", mData);
				
				resp.put("copytoweb", fdata);
			
			}else if(mFileCommand.equals(FILECOMMAND_DELETEFROMWEB)) {
				
				//Get the web Folder
				File webfiles = mMDS.getMiniDAPPWebFolder(mMiniDAPPID);
				
				//The new file..
				File webfile = new File(webfiles,mFile);
				
				//Does it exist
				boolean existed = webfile.exists();
				
				JSONObject fdata = new JSONObject();
				fdata.put("name", webfile.getName());
				fdata.put("existed", existed);
				
				MiniFile.deleteFileOrFolder(webfiles.getAbsolutePath(), webfile);
				
				resp.put("deletefromweb", fdata);
			
			}else if(mFileCommand.equals(FILECOMMAND_LISTWEB)) {
				
				//Get the web Folder
				File webfiles = mMDS.getMiniDAPPWebFolder(mMiniDAPPID);
				
				//The new file..
				File webfile = new File(webfiles,mFile);
				
				//Does it exist
				boolean exists = webfile.exists();
				
				JSONArray listfiles = new JSONArray();
				
				if(webfile.isDirectory() && exists) {
					//List the files..
					File[] files = webfile.listFiles();
					if(files == null) {
						files = new File[0];
					}
					
					//Sort the list
					Arrays.sort(files);
					for(File ff : files) {
						JSONObject fdata = new JSONObject();
						fdata.put("name", ff.getName());
						fdata.put("location", getCanonicalPath(webfiles,ff));
						fdata.put("size", ff.length());
						fdata.put("modified", ff.lastModified());
						fdata.put("isdir", ff.isDirectory());
						fdata.put("isfile", ff.isFile());
						listfiles.add(fdata);
					}
				}
			
				resp.put("exists", exists);
				resp.put("isdir", webfile.isDirectory());
				resp.put("list", listfiles);
			}
			
			JSONObject stattrue = new JSONObject();
			stattrue.put("command", "file");
			stattrue.put("status", true);
			stattrue.put("pending", false);
			stattrue.put("response", resp);
			result = stattrue.toJSONString();
			
		}catch(Exception exc) {
			//MinimaLogger.log("FILE command : "+mMiniDAPPID+" "+exc);
			
			statfalse.put("error", exc.toString());
			result = statfalse.toJSONString();
		}
		
		return result;
	}

	public String getCanonicalPath(File zRoot, File zFile) throws IOException {
		
		String rootcan 	= zRoot.getCanonicalPath();
		int rootlen 	= rootcan.length();
		
		String filecan 	= zFile.getCanonicalPath();
		filecan 		= filecan.substring(rootlen);
		filecan 		= filecan.replace("\\", "/");
		
		if(!filecan.startsWith("/")) {
			filecan = "/"+filecan;
		}
		
		//Double check
		filecan = filecan.replace("//", "/");
		
		return filecan;
	}
	
	static long download(String url, String fileName) throws IOException {
	    try (InputStream in = URI.create(url).toURL().openStream()) {
	        return Files.copy(in, Paths.get(fileName));
	    }
	}

}
