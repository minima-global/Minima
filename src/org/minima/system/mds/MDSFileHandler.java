package org.minima.system.mds;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.Socket;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.util.Date;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map;
import java.util.StringTokenizer;

import javax.net.ssl.SSLException;
import javax.net.ssl.SSLHandshakeException;

import org.minima.database.MinimaDB;
import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.mds.multipart.MultipartData;
import org.minima.system.mds.multipart.MultipartParser;
import org.minima.system.mds.polling.PollStack;
import org.minima.system.mds.publicmds.MDSPublicPage;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MiniFile;
import org.minima.utils.MinimaLogger;

public class MDSFileHandler implements Runnable {
	
	public static String MINIMA_DOWNLOAD_AS_FILE = "_minima_download_as_file_";
	
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
	 * MDS Command Handler
	 */
	MDSCommandHandler mCommands;
	
	/**
	 * Only show Invalid ID message once  every minute..
	 */
	static long mLastInvalidIDException = 0;
	
	/**
	 * The Public Pages
	 */
	MDSPublicPage mPublicMDS;
	
	/**
	 * Main Constructor
	 * @param zSocket
	 */
	public MDSFileHandler(File zRootFolder, Socket zSocket, MDSManager zMDS, PollStack zPStack) {
		//Store..
		mSocket 	= zSocket;
		mRoot 		= zRootFolder;
		mMDS 		= zMDS;
		mCommands 	= new MDSCommandHandler(mMDS, zPStack);
		mPublicMDS  = new MDSPublicPage(zMDS);
	}
	
	@Override
	public void run() {
		
		String fileRequested			= "";
		InputStream inputStream 		= null;
		OutputStream outputStream 		= null;
		BufferedReader bufferedReader	= null;
		DataOutputStream dos			= null;
		
		try {
			
			// Start handling application content
	        inputStream 	= mSocket.getInputStream();
	        outputStream 	= mSocket.getOutputStream();
	         
	        bufferedReader 	= new BufferedReader(new InputStreamReader(inputStream, MiniString.MINIMA_CHARSET));
	        dos 			= new DataOutputStream(outputStream);
	        
	        // get first line of the request from the client
	     	String input = bufferedReader.readLine();
 			
 			//Is it still NULL
 			if(input == null) {
 				return;
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
			fileRequested = URLDecoder.decode(fileRequested,"UTF-8").trim();
		
			//Get all the headers
			Hashtable<String, String> allheaders=new Hashtable<>();
			
			//Get the Headers
			while(input != null && !input.trim().equals("")) {
				int start    = input.indexOf(":");
				if(start != -1) {
					String name  = input.substring(0,start).trim();
					String value = input.substring(start+1).trim();
					
					//Put it in the headers
					allheaders.put(name, value);
				}
				
				//Read the next line..	
				input = bufferedReader.readLine();
			}
			
			//MinimaLogger.log("File Requested : "+fileRequested,false);
			
			//Is it the minihub..
			if(fileRequested.equals("")) {
				fileRequested = "index.html";
			}
			
			//Is it an MDS Command
			if(	fileRequested.startsWith("mdscommand_/") ) {
				
				//Get the command..
				fileRequested=fileRequested.substring(12); 
				
				//Get the command / params only
				int index 		= fileRequested.indexOf("?");
				String command 	= fileRequested.substring(0,index);
				String params 	= fileRequested.substring(index+1);
				
				//Get the UID
				String uid = "";
				StringTokenizer strtok = new StringTokenizer(params,"&");
				while(strtok.hasMoreElements()) {
					String tok = strtok.nextToken();
					
					index 			= tok.indexOf("=");
					String param 	= tok.substring(0,index);
					String value 	= tok.substring(index+1,tok.length());
					
					if(param.equals("uid")) {
						uid=value;
						break;
					}
				}
				
				//Convert the sessionid
				String minidappid = mMDS.convertSessionID(uid);
				if(minidappid == null) {
					throw new MDSInvalidIDException("Invalid session id for MiniDAPP "+uid);
				}
				
				//Are we resyncing..
				if(Main.getInstance().isShuttongDownOrRestoring() && !command.equals("poll")) {
					throw new MDSInvalidIDException("Attempt to access MDS during resync.. blocked");
				}

				/*if(Main.getInstance().isShuttongDownOrRestoring()) {
					
					//Check DB Open..
					if(!MinimaDB.getDB().getMDSDB().isOpen()) {
						throw new MDSInvalidIDException("Attempt to access MDS after shutdown");
					}
					
					//Only allow the security MiniDAPP..
					MiniDAPP mdcheck 	= mMDS.getMiniDAPP(minidappid);
					String namev 		= mdcheck.getName();
					
					//MinimaLogger.log("MDS DURING SHUTDOWN : command:"+command+" name:"+namev);
					
					if(namev.equalsIgnoreCase("minihub") || !command.equals("poll")) {
						MinimaLogger.log("Attempt to access MDS during resync from "+namev+" ..blocked", false);
						throw new MDSInvalidIDException("Attempt to access MDS during resync.. blocked");
					}
				}*/
				
				//get the POST data
				int contentlength = Integer.parseInt(allheaders.get("Content-Length"));
				
				//Read the data..
				char[] cbuf 	= new char[contentlength];
				
				//Read it ALL in
				int len,total=0;
				while( (len = bufferedReader.read(cbuf,total,contentlength-total)) != -1) {
					total += len;
					if(total == contentlength) {
						break;
					}
				}
				
				//It's an MDS command..
				String dataenc  = new String(cbuf).trim();
				String data 	= URLDecoder.decode(dataenc, "UTF-8");
				
				//Run IT!
				String result = mCommands.runCommand(minidappid, command, data);
				
				//The final data
				byte[] fdata = result.getBytes(MiniString.MINIMA_CHARSET);
				
				//Calculate the size of the response
				int finallength = fdata.length;
				
				// send HTTP Headers
				dos.writeBytes("HTTP/1.1 200 OK\r\n");
				dos.writeBytes("Server: Minima MDS Command server 1.3\r\n");
				dos.writeBytes("Date: " + new Date()+"\r\n");
				dos.writeBytes("Content-type: text/plain\r\n");
				dos.writeBytes("Content-length: " + finallength+"\r\n");
				dos.writeBytes("Access-Control-Allow-Origin: *\r\n");
				dos.writeBytes("\r\n"); // blank line between headers and content, very important !
				dos.write(fdata,0,finallength);
				dos.flush(); // flush character output stream buffer
				
			}else if(	fileRequested.equals("index.html") ||
						fileRequested.equals("invalid.html") ||
						fileRequested.equals("httperror.html") ||
						fileRequested.equals("noconnect.html") ||
						fileRequested.equals("favicon.png") ||
						fileRequested.equals("Manrope-Regular.ttf") ||
						fileRequested.equals("background.svg")) {
				
				writeHTMLResouceFile(dos, "hublogin/"+fileRequested);
				
			}else if(fileRequested.startsWith("logoff.html")){
				
				//Reset all session IDs..
				Main.getInstance().getMDSManager().PostMessage(MDSManager.MDS_MINIDAPPS_RESETSESSIONS);
				
				//Write the Main Login form
				writeHTMLResouceFile(dos, "hublogin/index.html");
				
			}else if(fileRequested.startsWith("login.html")){
				
				//Check the password
				Map params = getPostParams(allheaders, bufferedReader);
				
				//Check the Password..
				String password = "";
				if(params.containsKey("password")) {
					password = params.get("password").toString();
				}
				
				if(!mMDS.checkMiniHUBPasword(password)) {
					throw new IllegalArgumentException("Incorrect MDS Password");
				}
				
				//Get the default MiniHUB..
				String minihubuid = Main.getInstance().getMDSManager().getDefaultMiniHUB();
				
				//Load that MiniDFAPP..
				String base = "./"+minihubuid+"/index.html?uid="+mMDS.convertMiniDAPPID(minihubuid); 
				
				//Now load the success page
				String success = loadResouceFile("hublogin/success.html");
				
				//Replace the doRedirect()
				success = success.replace("function doRedirect(){}", 
										    "function doRedirect(){"
										  + "	window.location.href = \""+base+"\";"
										  + "}");
				
				//And write that out..
				writeHTMLPage(dos, success);
			
			}else if(fileRequested.startsWith("fileupload.html")){
				
				//get the POST data
				int contentlength = Integer.parseInt(allheaders.get("Content-Length"));
				
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
				
				//Get the bits..
				Hashtable<String, MultipartData> data = MultipartParser.parseMultipartData(alldata);
				
				//Which MiniDAPP..
				MultipartData minidappidpart = data.get("uid");
				if(minidappidpart==null) {
					throw new IllegalArgumentException("NO minidappuid specified in form for uploadfile");
				}
				String minidappsessionid = minidappidpart.getTextData();
				
				//Check it..
				String minidappid = mMDS.convertSessionID(minidappsessionid);
				if(minidappid == null) {
					throw new MDSInvalidIDException("Invalid session id for uploadfile "+minidappsessionid);
				}
				
				//Get the jumppage
				MultipartData jumppagepart = data.get("jumppage");
				if(jumppagepart==null) {
					throw new IllegalArgumentException("NO jumppage specified in form for uploadfile");
				}
				String jumppage = jumppagepart.getTextData();
				
				//Get the extradata
				MultipartData extradatapart = data.get("extradata");
				String extradata = "";
				if(extradatapart!=null) {
					extradata = URLEncoder.encode(extradatapart.getTextData(), "UTF-8");
				}
				
				//Now.. save the file..
				MultipartData filepart = data.get("fileupload"); 
				String filename 	= URLEncoder.encode(filepart.getFileName(), "UTF-8");
				String contenttype 	= URLEncoder.encode(filepart.getContentType(), "UTF-8");
				
				//Save the data
				byte[] filedata = filepart.getFileData();
				File root = new File(mMDS.getMiniDAPPFileFolder(minidappid),"fileupload");
				if(!root.exists()) {
					root.mkdirs();
				}
				MiniFile.writeDataToFile(new File(root,filepart.getFileName()), filedata);
				
				//Jump to the correct page..
				String base = "/"+minidappid+"/"+jumppage+"?uid="+minidappsessionid
						+"&fileupload="+filename
						+"&size="+filedata.length
						+"&contenttype="+contenttype;
				
				if(!extradata.equals("")) {
					base += "&extradata="+extradata;
				}
				
				//Create the web redirect paghe
				String webredirect = "<html>\r\n"
						+ "  <head>\r\n"
						+ "    <meta http-equiv='refresh' content=\"0; url='"+base+"\" />\r\n"
						+ "  </head>\r\n"
						+ "  <body>\r\n"
						+ "    <p>Please follow <a href='"+base+"'>this link</a>.</p>\r\n"
						+ "  </body>\r\n"
						+ "</html>";
				
				//Write this redirect page..
				writeHTMLPage(dos, webredirect);
				
			}else if(fileRequested.startsWith("fileuploadchunk.html")){
				
				//get the POST data
				int contentlength = Integer.parseInt(allheaders.get("Content-Length"));
				
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
				
				//Get the bits..
				Hashtable<String, MultipartData> data = MultipartParser.parseMultipartData(alldata);
				
				//Which MiniDAPP..
				MultipartData minidappidpart = data.get("uid");
				if(minidappidpart==null) {
					throw new IllegalArgumentException("NO minidappuid specified in form for uploadfile");
				}
				String minidappsessionid = minidappidpart.getTextData();
				
				//Check it..
				String minidappid = mMDS.convertSessionID(minidappsessionid);
				if(minidappid == null) {
					throw new MDSInvalidIDException("Invalid session id for uploadfile "+minidappsessionid);
				}
				
				//Now.. save the file..
				MultipartData filepart = data.get("fileupload"); 
				
				//Get other data
				String filename = data.get("filename").getTextData();
				int allchunks 	= Integer.parseInt(data.get("allchunks").getTextData());
				int chunk 	 	= Integer.parseInt(data.get("chunknum").getTextData());;
				
				//Save the data
				File root = new File(mMDS.getMiniDAPPFileFolder(minidappid),"fileupload");
				if(!root.exists()) {
					root.mkdirs();
				}
				File chunkroot 	= new File(root,"chunkupload");
				if(chunk == 0) {
					//First time..
					MiniFile.deleteFileOrFolder(root.getAbsolutePath(), chunkroot);
				}
				File chunkfile  = new File(chunkroot,"chunk_"+chunk);
				
				//Get the data
				byte[] filedata = filepart.getFileData();
				MiniFile.writeDataToFile(chunkfile, filedata);
				
				//Are we stitching it all together..
				if(chunk >= allchunks-1) {
					
					File finalfile = new File(root,filename);
					if(finalfile.exists()) {
						finalfile.delete();
					}
					
					for(int i=0;i<allchunks;i++) {
						File readchunkfile  = new File(chunkroot,"chunk_"+i);
						
						//Read in the complete file..
						byte[] chunkdata = MiniFile.readCompleteFile(readchunkfile);
						
						//And write it out..
						MiniFile.writeDataToFile(finalfile, chunkdata, true);
					}
					
					//And finally delete the chunk folder..
					MiniFile.deleteFileOrFolder(root.getAbsolutePath(), chunkroot);
				}
				
				//Write this page..
				dos.writeBytes("HTTP/1.0 200 OK\r\n");
				dos.writeBytes("Access-Control-Allow-Origin: *\r\n");
				dos.writeBytes("\r\n");
				dos.flush();
				
			}else if( fileRequested.startsWith("publicmds") ) {
				
				//Is the public site enabled..
				if(!MinimaDB.getDB().getUserDB().getPublicMDS()) {
					MinimaLogger.log("Access forbidden : Public MDS site disabled..!");
					
		    		dos.writeBytes("HTTP/1.0 404 OK\r\n");
					dos.writeBytes("\r\n");
					dos.flush();
					return;
				}
				
				//Is it the minihub..
				if(fileRequested.equals("publicmds")) {
					fileRequested = "publicmds/index.html";
				}
				
				if(fileRequested.equals("publicmdsgen")) {
					fileRequested = "publicmdsgen/index.html";
				}
				
				//Remove the params..
				int index = fileRequested.indexOf("?");
				if(index!=-1) {
					fileRequested = fileRequested.substring(0,index);
				}
				
				if(fileRequested.equals("publicmds/index.html")) {
					
					//Set the session ID
					String success = loadResouceFile("publicmds/index.html");
					
					//Get the public sessionID
					String seshid = mMDS.getPublicMiniDAPPSessionID();
					
					//Replace the doRedirect()
					success = success.replace("var publicsessionid=\"0x00\";", 
											   "var publicsessionid=\""+seshid+"\";");
					
					//Do we enable the Wallet..
					if(GeneralParams.IS_MEGAMMR) {
						success = success.replace("var showwallet=false","var showwallet=true");
					}
					
					//And write that out..
					writeHTMLPage(dos, success);
				
				}else if(fileRequested.equals("publicmdsgen/index.html")) {
					
					//Use the PublicMDS generator..
					String publicindex = mPublicMDS.getIndexPage();
					
					//And write that out..
					writeHTMLPage(dos, publicindex);
				
				}else if(fileRequested.endsWith("/mds.js")) {
				
					//Always send the latest version..
					writeHTMLResouceFile(dos, "mdsjs/mds.js");
			
				}else {
					//Write this page..
					writeHTMLResouceFile(dos, fileRequested);
				}
				
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
				
				//Are we asking for mds.js..
				if(webfile.getName().equalsIgnoreCase("mds.js")) {
					//MinimaLogger.log("MDSJS OVERRIDE : "+fileRequested);
					
					//Always send the latest version..
					writeHTMLResouceFile(dos, "mdsjs/mds.js");
				
				}else {
					
					//Check is valid child of parent..
					boolean ischild = MiniFile.isChild(mRoot, webfile);
					
					if(!webfile.exists() || !ischild || webfile.isDirectory()) {
			    		
			    		MinimaLogger.log("HTTP : unknown file requested "+fileRequested+" "+webfile.getAbsolutePath());
			    		
			    		dos.writeBytes("HTTP/1.0 404 OK\r\n");
						dos.writeBytes("\r\n");
						dos.flush();
			    		
			    	}else {
			    		
			    		//MinimaLogger.log("File Requested : "+fileRequested,false);
			    		
			    		boolean downloader 	= false;
			    		String filename 	= webfile.getName();
			    		if(filename.contains(MINIMA_DOWNLOAD_AS_FILE)){
			    			//Remove the ending..
			    			filename 	= filename.replace(MINIMA_DOWNLOAD_AS_FILE, "");
			    			downloader 	= true;
			    		}
			    		
			    		//Get the data length
			    		long filelen = webfile.length();
			    		
						//Calculate the size of the response
						dos.writeBytes("HTTP/1.0 200 OK\r\n");
						if(contenttype.startsWith("text/")) {
							dos.writeBytes("Content-Type: "+contenttype+"; charset=UTF-8\r\n");
						}else {
							dos.writeBytes("Content-Type: "+contenttype+"\r\n");
						}
						
						dos.writeBytes("Content-Length: " + filelen+ "\r\n");
						dos.writeBytes("Access-Control-Allow-Origin: *\r\n");
						
						//Only cache Images ?
						if(contenttype.startsWith("image") || contenttype.endsWith("css")) {
							dos.writeBytes("Cache-Control: max-age=604800: *\r\n");
						}
								
						//Are we downloading this file..
						if(downloader) {
							dos.writeBytes("Content-Disposition: attachment; filename=\""+filename+"\"\r\n");
						}
						
						//End Headers..
						dos.writeBytes("\r\n");
						
						//Now write the data out.. stream..
						FileInputStream fis = new FileInputStream(webfile);
						byte[] buffer 		= new byte[32768];
				        int length;
				        while ((length = fis.read(buffer)) > 0) {
				        	dos.write(buffer, 0, length);
				        }
					    fis.close();
				        
						//Flush the stream
						dos.flush();
			    	}	
				}				
			}
		
		}catch(SSLHandshakeException exc) {
		}catch(SSLException exc) {
		}catch(MDSCommandException exc) {
			MinimaLogger.log("MDSCommandException : "+exc.toString());
			
			// send HTTP Headers
			try {
				dos.writeBytes("HTTP/1.1 500 Internal Server Error\r\n");
				dos.writeBytes("Server: HTTP MDS Server from Minima 1.3\r\n");
				dos.writeBytes("Date: " + new Date()+"\r\n");
				dos.writeBytes("Content-type: text/plain\r\n");
				dos.writeBytes("Access-Control-Allow-Origin: *\r\n");
				dos.writeBytes("\r\n"); // blank line between headers and content, very important !
				dos.flush(); // flush character output stream buffer
			}catch (Exception e) {
				// TODO: handle exception
			}
			
		}catch(MDSInvalidIDException exc) {
			
			//Only show every so often..
			long timenow 	= System.currentTimeMillis();
			long lasterror 	= timenow - mLastInvalidIDException;
			if(lasterror > 120 * 1000) {
				MinimaLogger.log("MDS Invalid SessionID Exception : You need to close your MiniDAPPS, logout of the MDS and then log back in..");
				mLastInvalidIDException = timenow;
			}
			
			// send HTTP Headers
			try {
				dos.writeBytes("HTTP/1.1 500 Internal Server Error\r\n");
				dos.writeBytes("Server: HTTP MDS Server from Minima 1.3\r\n");
				dos.writeBytes("Date: " + new Date()+"\r\n");
				dos.writeBytes("Content-type: text/plain\r\n");
				dos.writeBytes("Access-Control-Allow-Origin: *\r\n");
				dos.writeBytes("\r\n"); // blank line between headers and content, very important !
				dos.flush(); // flush character output stream buffer
			}catch (Exception e) {
				// TODO: handle exception
			}
			
		}catch(IllegalArgumentException exc) {
			
			MinimaLogger.log(exc);
			
			//Write out an error page
			if(dos !=null) {
				try {
					writeHTMLResouceFile(dos, "hublogin/invalid.html");
					//writeHTMLPage(dos, MDSHubError.createHubPage());
				} catch (IOException e) {}
			}
			
		}catch(Exception exc) {
			
			//MinimaLogger.log(exc);
			
			//Write out an error page
			if(dos !=null) {
				try {
					writeHTMLResouceFile(dos, "hublogin/invalid.html");
					//writeHTMLPage(dos, MDSHubError.createHubPage());
				} catch (IOException e) {}
			}
			
		}finally {
			try {
				if(bufferedReader!=null) {
					bufferedReader.close();
				}
				
				if(inputStream!=null) {
					inputStream.close();
				}
				
				if(dos!=null) {
					dos.close();
				}
				
				if(outputStream!=null) {
					outputStream.close();
				}
				
				mSocket.close(); // we close socket connection
				
			} catch (Exception e) {
				//MinimaLogger.log(e);
			} 	
		}	
	}	
	
	private static Map<String, String> getQueryMap(String query) {  
	    String[] params 		= query.split("&");  
	    Map<String, String> map = new HashMap<String, String>();

	    try {
	    	for (String param : params) {  
		        String name = param.split("=")[0];  
		        String value = param.split("=")[1];
		        
		        //URL decode.
		        value = URLDecoder.decode(value,"UTF-8").trim();
		        
		        map.put(name, value);  
		    }
	    }catch(Exception exc) {
	    	map = new HashMap<String, String>();
	    }
	      
	    return map;  
	}
	
	private Map getPostParams(Hashtable<String, String> zHeaders, BufferedReader bufferedReader) throws Exception {
		
		//How much content
		int contentlength = Integer.parseInt(zHeaders.get("Content-Length"));
		
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
		
		//Get all the params
		Map params  = getQueryMap(new String(cbuf));
		
		return params;
	}
	
	public void writeHTMLPage(DataOutputStream zDos, String zWebPage) throws IOException {
		//It's the root file..
		byte[] file = zWebPage.getBytes();

		//Calculate the size of the response
		int finallength = file.length;
        
		zDos.writeBytes("HTTP/1.0 200 OK\r\n");
		zDos.writeBytes("Content-Type: text/html\r\n");
		zDos.writeBytes("Content-Length: " + finallength + "\r\n");
		zDos.writeBytes("Access-Control-Allow-Origin: *\r\n");
		zDos.writeBytes("\r\n");
		zDos.write(file, 0, finallength);
		zDos.flush();
	}
	
	public void writeHTMLResouceFile(DataOutputStream zDos, String zResource) throws IOException {
		
		//Now get the content type
		String contenttype 	= MiniFile.getContentType(zResource);
		
		//Get the Resource file
		InputStream is 	= getClass().getClassLoader().getResourceAsStream(zResource);
		
		//Get all the data..
		byte[] file = MiniFile.readAllBytes(is);
		is.close();
		
		//Calculate the size of the response
		int finallength = file.length;
        
		zDos.writeBytes("HTTP/1.0 200 OK\r\n");
		
		if(contenttype.startsWith("text/")) {
			zDos.writeBytes("Content-Type: "+contenttype+"; charset=UTF-8\r\n");
		}else {
			zDos.writeBytes("Content-Type: "+contenttype+"\r\n");
		}
		//zDos.writeBytes("Content-Type: text/html\r\n");
		
		zDos.writeBytes("Content-Length: " + finallength + "\r\n");
		zDos.writeBytes("Access-Control-Allow-Origin: *\r\n");
		zDos.writeBytes("\r\n");
		zDos.write(file, 0, finallength);
		zDos.flush();
	}
	
	public String loadResouceFile(String zResource) throws IOException {
		
		//Get the Resource file
		InputStream is 	= getClass().getClassLoader().getResourceAsStream(zResource);
		
		//Get all the data..
		byte[] file = MiniFile.readAllBytes(is);
		is.close();
		
		return new String(file, MiniString.MINIMA_CHARSET);
	}
}
