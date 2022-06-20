package org.minima.system.mds;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.Socket;
import java.net.URLDecoder;
import java.util.StringTokenizer;

import org.minima.objects.base.MiniString;
import org.minima.system.mds.hub.MDSHub;
import org.minima.system.mds.hub.MDSHubError;
import org.minima.system.mds.hub.MDSHubLogon;
import org.minima.utils.MiniFile;
import org.minima.utils.MinimaLogger;

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
	 * Main Constructor
	 * @param zSocket
	 */
	public MDSFileHandler(File zRootFolder, Socket zSocket) {
		//Store..
		mSocket = zSocket;
		mRoot 	= zRootFolder;
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
			if (input == null){
				input = "";
			}
			
			//Get the first line..
			String firstline = new String(input);
			if(firstline.trim().equals("")) {
				//Nothing to do..
				inputStream.close();
				outputStream.close();
				mSocket.close();
				return;
			}
			
			// we parse the request with a string tokenizer
			StringTokenizer parse = new StringTokenizer(input);
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
				dos.writeBytes("\r\n");
				dos.write(file, 0, finallength);
				dos.flush();
			
			}else if(fileRequested.startsWith("login.html")){
				
				//Get the password..
				String pass 	= getPassword(fileRequested);
				
				//Check this is the correct password..
				String webpage = null;
				if(!pass.equals("minima")) {
					MinimaLogger.log("Incorrect Password : "+pass);
					webpage 	= MDSHubError.createHubPage();
				}else {
					webpage 	= MDSHub.createHubPage();
				}
				
				//It's the root file..
				byte[] file = webpage.getBytes();
	
				//Calculate the size of the response
				int finallength = file.length;
	            
				dos.writeBytes("HTTP/1.0 200 OK\r\n");
				dos.writeBytes("Content-Type: text/html\r\n");
				dos.writeBytes("Content-Length: " + finallength + "\r\n");
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
	
				if(!webfile.exists()) {
		    		
		    		MinimaLogger.log("HTTP : unknown file requested "+fileRequested+" "+webfile.getAbsolutePath());
		    		
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
					dos.writeBytes("\r\n");
					dos.write(file, 0, finallength);
					dos.flush();
		    	}
			}
			
		}catch(Exception exc) {
			MinimaLogger.log(exc);
			
		}finally {
			try {
				inputStream.close();
				outputStream.close();
				mSocket.close(); // we close socket connection
			} catch (Exception e) {
				MinimaLogger.log(e);
			} 	
		}	
	}	
	
	private String getPassword(String zURL) {
		
		int index = zURL.indexOf("?");
		if(index != -1) {
			
			String fullpass = zURL.substring(index+1);
			index 			= fullpass.indexOf("=");
			String pass 	= fullpass.substring(index+1); 
			
			return pass;
		}
		
		return "";
	}
}
