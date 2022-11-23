package org.minima.system.mds;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.Socket;
import java.net.URLDecoder;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;

import javax.net.ssl.SSLException;
import javax.net.ssl.SSLHandshakeException;

import org.minima.database.MinimaDB;
import org.minima.database.minidapps.MDSDB;
import org.minima.database.minidapps.MiniDAPP;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.commands.CommandException;
import org.minima.system.mds.handler.CMDcommand;
import org.minima.system.mds.hub.MDSHub;
import org.minima.system.mds.hub.MDSHubDelete;
import org.minima.system.mds.hub.MDSHubError;
import org.minima.system.mds.hub.MDSHubInstall;
import org.minima.system.mds.hub.MDSHubLogon;
import org.minima.system.mds.hub.MDSHubPending;
import org.minima.system.mds.hub.MDSHubPendingAction;
import org.minima.utils.MiniFile;
import org.minima.utils.MinimaLogger;
import org.minima.utils.ZipExtractor;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.messages.Message;

public class MDSFileHandler implements Runnable {
	
	/**
	 * The Net Socket
	 */
	Socket mSocket;
	
	/**
	 * The Root Folder
	 */
	File mRoot;
	
	/**
	 * The MDS Manager
	 */
	MDSManager mMDS;
	
	/**
	 * Main Constructor
	 * @param zSocket
	 */
	public MDSFileHandler(File zRootFolder, Socket zSocket, MDSManager zMDS) {
		//Store..
		mSocket = zSocket;
		mRoot 	= zRootFolder;
		mMDS 	= zMDS;
	}
	
	@Override
	public void run() {
		
		String fileRequested		= "";
		InputStream inputStream 	= null;
		OutputStream outputStream 	= null;
		
		try {
			
			// Start handling application content
	        inputStream 	= mSocket.getInputStream();
	        outputStream 	= mSocket.getOutputStream();
	         
	        BufferedReader bufferedReader 	= new BufferedReader(new InputStreamReader(inputStream, MiniString.MINIMA_CHARSET));
	        DataOutputStream dos 			= new DataOutputStream(outputStream);
	        
	        // get first line of the request from the client
	     	String input = bufferedReader.readLine();
 			int counter = 0;
 			while(input == null && counter<100){
 				//Wait a sec
 				Thread.sleep(1000);
 				
 				input = bufferedReader.readLine();
 				counter++;
 			}
 			
 			//Is it still NULL
 			if(input == null) {
 				throw new IllegalArgumentException("Invalid NULL MDS File request ");
 			}
 			
			// we parse the request with a string tokenizer
			StringTokenizer parse = new StringTokenizer(input);
			
			//Get the method..
			String method = parse.nextToken().toUpperCase(); // we get the HTTP method of the client
			
			// we get file requested
			fileRequested = parse.nextToken();
			
			//Remove slashes..
			if(fileRequested.startsWith("/")) {
				fileRequested = fileRequested.substring(1);
			}
			if(fileRequested.endsWith("/")) {
				fileRequested = fileRequested.substring(0,fileRequested.length()-1);
			}
			
			//And finally URL decode..
			fileRequested 		= URLDecoder.decode(fileRequested,"UTF-8").trim();
			
			if(fileRequested.equals("")) {
				
				String webpage = MDSHubLogon.createHubPage();
				
				//It's the root file..
				byte[] file = webpage.getBytes();
	
				//Calculate the size of the response
				int finallength = file.length;
	            
				dos.writeBytes("HTTP/1.0 200 OK\r\n");
				dos.writeBytes("Content-Type: text/html\r\n");
				dos.writeBytes("Content-Length: " + finallength + "\r\n");
				dos.writeBytes("Access-Control-Allow-Origin: *\r\n");
				dos.writeBytes("\r\n");
				dos.write(file, 0, finallength);
				dos.flush();
			
			}else if(fileRequested.startsWith("login.html")){
			
				Map params  = checkPostPassword(input, bufferedReader, inputStream);
				String webpage = null;
				if(params == null) {
					webpage 	= MDSHubError.createHubPage();
				}else {
					String pass = params.get("password").toString();
					webpage 	= MDSHub.createHubPage(mMDS, pass);
				}
				
				//It's the root file..
				byte[] file = webpage.getBytes();
	
				//Calculate the size of the response
				int finallength = file.length;
	            
				dos.writeBytes("HTTP/1.0 200 OK\r\n");
				dos.writeBytes("Content-Type: text/html\r\n");
				dos.writeBytes("Content-Length: " + finallength + "\r\n");
				dos.writeBytes("Access-Control-Allow-Origin: *\r\n");
				dos.writeBytes("\r\n");
				dos.write(file, 0, finallength);
				dos.flush();
	
			}else if(fileRequested.startsWith("install.html")){
				
				//get the POST data
				int contentlength = 0;
				while(input != null && !input.trim().equals("")) {
					int ref = input.indexOf("Content-Length:"); 
					if(ref != -1) {
						//Get it..
						int start     = input.indexOf(":");
						contentlength = Integer.parseInt(input.substring(start+1).trim());
					}	
					input = bufferedReader.readLine();
				}
				
				//Read the data..
				byte[] alldata = new byte[contentlength];
				
				//Read it ALL in
				int len,total=0;
				while( (len = inputStream.read(alldata,total,contentlength-total)) != -1) {
					total += len;
					if(total == contentlength) {
						break;
					}
				}
				
				//Create an input stream for the file..
				ByteArrayInputStream bais 	= new ByteArrayInputStream(alldata);
				DataInputStream dis 		= new DataInputStream(bais);
				
				//FIRST read in the password..
				String line = dis.readLine();
				while(!line.equals("")) {
					line = dis.readLine();
				}
				
				//Password is the next line..
				String password = dis.readLine();
				if(!mMDS.checkMiniHUBPasword(password)) {
					MinimaLogger.log("Incorrect Install MiniDAPP Password : "+password);
					throw new IllegalArgumentException("Invalid Password");
				}
				
				//Now read lines until we reach the data
				line = dis.readLine();
				while(!line.equals("")) {
					line = dis.readLine();
				}
				
				//Where is it going..
				String rand = MiniData.getRandomData(16).to0xString();
				
				//The file where the package is extracted..
				File dest 	= new File( Main.getInstance().getMDSManager().getWebFolder() , rand);
				if(dest.exists()) {
					MiniFile.deleteFileOrFolder(dest.getAbsolutePath(), dest);
				}
				boolean mk = dest.mkdirs();
				
				//Send it to the extractor..
				ZipExtractor.unzip(dis, dest);
				bais.close();
				
				//Is there a conf file..
				File conf = new File(dest,"dapp.conf");
				if(!conf.exists()) {
					//Delete the install
					MiniFile.deleteFileOrFolder(dest.getAbsolutePath(), dest);	
					throw new CommandException("No dapp.conf file found @ "+conf.getAbsolutePath());
				}
				
				//Load the Conf file.. to get the data
				MiniString data = new MiniString(MiniFile.readCompleteFile(conf)); 	
				
				//Now create the JSON..
				JSONObject jsonconf = (JSONObject) new JSONParser().parse(data.toString());
				
				//ALWAYS starts with only READ Permission
				jsonconf.put("permission", "read");
				
				//Create the MiniDAPP
				MiniDAPP md = new MiniDAPP(rand, jsonconf);
				
				//Now add to the DB
				MDSDB db = MinimaDB.getDB().getMDSDB();
				db.insertMiniDAPP(md);
				
				//There has been a change
				Message installed = new Message(MDSManager.MDS_MINIDAPPS_INSTALLED);
				installed.addObject("minidapp", md);
				Main.getInstance().getMDSManager().PostMessage(installed);
				
				//Create the webpage
				String webpage = MDSHubInstall.createHubPage(mMDS,md,password);
		
				//Get the data
				byte[] file = webpage.getBytes(MiniString.MINIMA_CHARSET);
	
				//Calculate the size of the response
				int finallength = file.length;
	
				dos.writeBytes("HTTP/1.0 200 OK\r\n");
				dos.writeBytes("Content-Type: text/html\r\n");
				dos.writeBytes("Content-Length: " + finallength + "\r\n");
				dos.writeBytes("Access-Control-Allow-Origin: *\r\n");
				dos.writeBytes("\r\n");
				dos.write(file, 0, finallength);
				dos.flush();
	
			}else if(fileRequested.startsWith("pending.html")){
				
				Map params  = checkPostPassword(input, bufferedReader, inputStream);
				String webpage = null;
				if(params == null) {
					webpage 	= MDSHubError.createHubPage();
				}else {
					String pass = params.get("password").toString();
					webpage 	= MDSHubPending.createHubPage(mMDS, pass);
				}
				
				//Get the data
				byte[] file = webpage.getBytes(MiniString.MINIMA_CHARSET);
	
				//Calculate the size of the response
				int finallength = file.length;
	
				dos.writeBytes("HTTP/1.0 200 OK\r\n");
				dos.writeBytes("Content-Type: text/html\r\n");
				dos.writeBytes("Content-Length: " + finallength + "\r\n");
				dos.writeBytes("Access-Control-Allow-Origin: *\r\n");
				dos.writeBytes("\r\n");
				dos.write(file, 0, finallength);
				dos.flush();
			
			}else if(fileRequested.startsWith("pendingaction.html")){
				
				Map params  	= checkPostPassword(input, bufferedReader, inputStream);
				if(params == null) {
					throw new IllegalArgumentException("Invalid Password");
				}
				
				String password = params.get("password").toString();
				String accept 	= params.get("accept").toString();
				String uid 		= params.get("uid").toString();
				
				String webpage=null;
				if(accept.equals("accept")) {
					
					//Run this command
					CMDcommand cmd = new CMDcommand("0x00", "mds action:accept uid:"+uid);
					String result = cmd.runCommand();
					
					//Run this command..
					webpage = MDSHubPendingAction.createHubPage(mMDS, password, true, result);
					
				}else {
					
					//Run this command
					CMDcommand cmd = new CMDcommand("0x00", "mds action:deny uid:"+uid);
					String result = cmd.runCommand();
					
					//Deny this command
					webpage = MDSHubPendingAction.createHubPage(mMDS, password, false, result);
				}
				
				//Get the data
				byte[] file = webpage.getBytes(MiniString.MINIMA_CHARSET);
	
				//Calculate the size of the response
				int finallength = file.length;
	
				dos.writeBytes("HTTP/1.0 200 OK\r\n");
				dos.writeBytes("Content-Type: text/html\r\n");
				dos.writeBytes("Content-Length: " + finallength + "\r\n");
				dos.writeBytes("Access-Control-Allow-Origin: *\r\n");
				dos.writeBytes("\r\n");
				dos.write(file, 0, finallength);
				dos.flush();
				
			}else if(fileRequested.startsWith("delete.html")){
				
				Map params  	= checkPostPassword(input, bufferedReader, inputStream);
				if(params == null) {
					throw new IllegalArgumentException("Invalid Password");
				}
				
				String password = params.get("password").toString();
				String uid 		= params.get("uid").toString();
				
				//Now add to the DB
				MDSDB db = MinimaDB.getDB().getMDSDB();
				db.deleteMiniDAPP(uid);
				
				// Delete web..
				String mdsroot 	= Main.getInstance().getMDSManager().getRootMDSFolder().getAbsolutePath();
				File dest 		= Main.getInstance().getMDSManager().getWebFolder();
				File minidapp 	= new File(dest,uid);
				if(minidapp.exists()) {
					MiniFile.deleteFileOrFolder(mdsroot, minidapp);
				}
				
				//Delete Data folder
				Main.getInstance().getMDSManager().shutdownSQL(uid);
				File dbfolder1 = Main.getInstance().getMDSManager().getMiniDAPPDataFolder(uid);
				if(dbfolder1.exists()) {
					MiniFile.deleteFileOrFolder(mdsroot, dbfolder1);
				}
				
				//There has been a change
				Message uninstall = new Message(MDSManager.MDS_MINIDAPPS_UNINSTALLED);
				uninstall.addString("uid", uid);
				Main.getInstance().getMDSManager().PostMessage(uninstall);
				
				//Create the webpage
				String webpage = MDSHubDelete.createHubPage(mMDS, password);
		
				//Get the data
				byte[] file = webpage.getBytes(MiniString.MINIMA_CHARSET);
	
				//Calculate the size of the response
				int finallength = file.length;
	
				dos.writeBytes("HTTP/1.0 200 OK\r\n");
				dos.writeBytes("Content-Type: text/html\r\n");
				dos.writeBytes("Content-Length: " + finallength + "\r\n");
				dos.writeBytes("Access-Control-Allow-Origin: *\r\n");
				dos.writeBytes("\r\n");
				dos.write(file, 0, finallength);
				dos.flush();
	
			}else {
			
				//Remove the params..
				int index = fileRequested.indexOf("?");
				if(index!=-1) {
					fileRequested = fileRequested.substring(0,index);
				}
			
				//Now get the content type
				String contenttype 	= MiniFile.getContentType(fileRequested);
				
				//Now get the file..
				File webfile = new File(mRoot, fileRequested);
				
				//Check is valid child of parent..
				boolean ischild = MiniFile.isChild(mRoot, webfile);
				
				if(!webfile.exists() || !ischild || webfile.isDirectory()) {
		    		
		    		//MinimaLogger.log("HTTP : unknown file requested "+fileRequested+" "+webfile.getAbsolutePath());
		    		
		    		dos.writeBytes("HTTP/1.0 404 OK\r\n");
					dos.writeBytes("\r\n");
					dos.flush();
		    		
		    	}else {
		    		
		    		//Get the data
					byte[] file = MiniFile.readCompleteFile(webfile);
		
					//Calculate the size of the response
					int finallength = file.length;
		            
					dos.writeBytes("HTTP/1.0 200 OK\r\n");
					dos.writeBytes("Content-Type: "+contenttype+"\r\n");
					dos.writeBytes("Content-Length: " + finallength + "\r\n");
					dos.writeBytes("Access-Control-Allow-Origin: *\r\n");
					dos.writeBytes("\r\n");
					dos.write(file, 0, finallength);
					dos.flush();
		    	}
			}
		
		}catch(SSLHandshakeException exc) {
		}catch(SSLException exc) {
		}catch(IllegalArgumentException exc) {
		}catch(Exception exc) {
			MinimaLogger.log(exc);
			
		}finally {
			try {
				inputStream.close();
				outputStream.close();
				mSocket.close(); // we close socket connection
			} catch (Exception e) {
//				MinimaLogger.log(e);
			} 	
		}	
	}	
	
	public static Map<String, String> getQueryMap(String query) {  
	    String[] params = query.split("&");  
	    Map<String, String> map = new HashMap<String, String>();

	    for (String param : params) {  
	        String name = param.split("=")[0];  
	        String value = param.split("=")[1];  
	        map.put(name, value);  
	    }  
	    return map;  
	}
	
	public Map checkPostPassword(String input, BufferedReader bufferedReader, InputStream inputStream) throws Exception {
		//PASSWORD passed in POST data
		int contentlength = 0;
		while(input != null && !input.trim().equals("")) {
			//MinimaLogger.log("RPC : "+input);
			int ref = input.indexOf("Content-Length:"); 
			if(ref != -1) {
				//Get it..
				int start     = input.indexOf(":");
				contentlength = Integer.parseInt(input.substring(start+1).trim());
			}	
			input = bufferedReader.readLine();
		}
		
		//How much data
		char[] cbuf 	= new char[contentlength];
		
		//Read it ALL in
		int len,total=0;
		while( (len = bufferedReader.read(cbuf,total,contentlength-total)) != -1) {
			total += len;
			if(total == contentlength) {
				break;
			}
		}
		
		//Here is the login attempt
		Map params  	= getQueryMap(new String(cbuf));
		String password = params.get("password").toString();
		
		if(!mMDS.checkMiniHUBPasword(password)) {
			MinimaLogger.log("Incorrect MiniDAPP Password : "+password);
			return null;
		}
		
		return params;
	}
}
