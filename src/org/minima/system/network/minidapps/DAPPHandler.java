package org.minima.system.network.minidapps;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.net.Socket;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Date;
import java.util.StringTokenizer;

import org.minima.objects.base.MiniData;
import org.minima.system.backup.BackupManager;
import org.minima.system.network.minidapps.hexdata.faviconico;
import org.minima.system.network.minidapps.hexdata.helphtml;
import org.minima.system.network.minidapps.hexdata.iconpng;
import org.minima.system.network.minidapps.hexdata.indexhtml;
import org.minima.system.network.minidapps.hexdata.installdapphtml;
import org.minima.system.network.minidapps.hexdata.minidappscss;
import org.minima.system.network.minidapps.hexdata.tilegreyjpeg;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

/**
 * This class handles a single request then exits
 * 
 * @author spartacusrex
 *
 */
public class DAPPHandler implements Runnable {

	/**
	 * The Net Socket
	 */
	Socket mSocket;
	
	DAPPManager mDAPPManager;
	
	/**
	 * Load from resource or from the STATIC hard coded files
	 */
	boolean mUseResources = false;
	
	/**
	 * Main Constructor
	 * @param zSocket
	 */
	public DAPPHandler(Socket zSocket, DAPPManager zDAPPManager) {
		//Store..
		mSocket      = zSocket;
		mDAPPManager = zDAPPManager;
	}

	@Override
	public void run() {
		// we manage our particular client connection
		BufferedReader in 	 		 	= null; 
		PrintStream out 	 			= null; 
		
		String fileRequested 			= null;
		
		try {
			// Input Stream
			InputStreamReader is = new InputStreamReader(mSocket.getInputStream());
			in = new BufferedReader(is);
			
			// Output Stream
			out = new PrintStream(mSocket.getOutputStream());
			
			// get first line of the request from the client
			String input = in.readLine();
			if(input == null) {
				throw new Exception("ZERO Input Request..");
			}
			String firstline = input;
			
			//Content headers..
			int contentlength = 0;
			String boundary = "";
			while (input!=null && !input.isEmpty()) { 
				//System.out.println(input);
				
				//Find the Length..
				if(input.indexOf("Content-Length:") != -1) {
					String contlen = input.substring(15);
//					System.out.println("LENGTH : "+contlen);
					contentlength = Integer.parseInt(contlen.trim());
				}
				
				//Find the Boundary if required in POST
				int bound = input.indexOf("boundary=");
				if(bound != -1) {
					boundary = input.substring(bound+9);
//					System.out.println("BOUNDARY : "+boundary);
				}
				input = in.readLine(); 
			}
			
			// we parse the request with a string tokenizer
			StringTokenizer parse = new StringTokenizer(firstline);
			String method = parse.nextToken().toUpperCase(); // we get the HTTP method of the client
			
			// we get file requested
			fileRequested = parse.nextToken();
			
//			System.out.println("GET "+fileRequested);
			
			if(fileRequested.endsWith("/")) {
				fileRequested = fileRequested.concat("index.html");
			}
			
			if(fileRequested.startsWith("/")) {
				fileRequested = fileRequested.substring(1);
			}
			
			//decode URL message
			fileRequested = URLDecoder.decode(fileRequested,"UTF-8").trim();
			
			//Remove anything after the ?
			int questionmark = fileRequested.indexOf("?");
			if(questionmark != -1) {
				String query  = fileRequested.substring(questionmark+1);
				fileRequested = fileRequested.substring(0,questionmark);
			
				//Are we uninstalling..
				if(fileRequested.equals("index.html") && !query.contains("..")) {
					//Is it uninstall..
					int uninstall = query.indexOf("uninstall=");
					if(uninstall != -1) {
						//Create the complete folder..
						File appfolder = new File(mDAPPManager.getMiniDAPPSFolder(),query.substring(uninstall+10));
						
//						//Check is the actual folder..
//						String parent = appfolder.getParentFile().getName();
//						if(!parent.equals("minidapps")) {
//							appfolder = appfolder.getParentFile();
//						}
						
						//Delete the app root..
						BackupManager.deleteFolder(appfolder);
						
						//Recalculate the MINIDAPPS
						mDAPPManager.recalculateMiniDAPPS();
					}
				}
			}
			
			//Get the File.. index.html is a resource.. everything else is hosted in the minidapps folder
			byte[] file = null;
			int filelen = 0;
			
			if(fileRequested.endsWith("minima.js")) {
				file    = getResourceBytes("js/minima.js");
				filelen = file.length;
				
			}else if(fileRequested.startsWith("minidapps/")) {
				//Look in the minidapps folder
				String fullfile = mDAPPManager.getMiniDAPPSFolder()+"/"+fileRequested.substring(10);
				file    = getFileBytes(fullfile);
				filelen = file.length;
			
			}else if(fileRequested.startsWith("rpc/")) {
				//It's an RPC request
				
			}else {
				if(!mUseResources) {
					if(fileRequested.equals("index.html")) {
						String page    = new String(indexhtml.returnData(),StandardCharsets.UTF_8);
						String newpage = page.replace("######", createMiniDAPPList());
						file = newpage.getBytes();
						
					}else if(fileRequested.equals("css/minidapps.css")) {
						file    = minidappscss.returnData();
					
					}else if(fileRequested.equals("favicon.ico")) {
						file    = faviconico.returnData();
					
					}else if(fileRequested.equals("help.html")) {
						file    = helphtml.returnData();
					
					}else if(fileRequested.equals("icon.png")) {
						file    = iconpng.returnData();
					
					}else if(fileRequested.equals("installdapp.html")) {
						file    = installdapphtml.returnData();
					
					}else if(fileRequested.equals("tile-grey.jpeg")) {
						file    = tilegreyjpeg.returnData();
						
					}else {
						//Not found..
						file    = new byte[0];	
					}	
					
				}else {
					if(fileRequested.equals("index.html")) {
						file           = getResourceBytes(fileRequested);
						String page    = new String(file,StandardCharsets.UTF_8);
						String newpage = page.replace("######", createMiniDAPPList());
						file           = newpage.getBytes();
					}else {
						file = getResourceBytes(fileRequested);	
					}	
				}
				
				filelen = file.length;
			}
			
			//Now serve the Page
			int dot        = fileRequested.lastIndexOf(".");
			String content = "text/plain";
			if(dot != -1) {
				content = getContentType(fileRequested.substring(dot+1));
			}
			
			//Did we find it.. ?
			if(filelen == 0) {
				
				out.println("HTTP/1.1 404 Not Found");
				out.println("Server: HTTP Server from Minima : 1.0");
				out.println("Date: " + new Date());
				out.println("Content-type: text/html");
				out.println("Content-length: 0");
				out.println("Access-Control-Allow-Origin: *");
				out.println(); // blank line between headers and content, very important !
				out.flush(); // flush character output stream buffer
			
			}else{

				//POST requests 
				if (method.equals("POST")){
					//The data buffer for all the data 
					char[] alldata = new char[contentlength]; 
					in.read(alldata, 0, contentlength);
					
					String complete = new String(alldata);
					
					//Decode..
					String decoded = URLDecoder.decode(complete,"UTF-8");
					
					//Base64 decode..
					int index = decoded.indexOf("base64,");
					
					//Just the Base64 bit
					String data = decoded.substring(index+7);
					
					//Convert to Byte..
					byte[] bdata = Base64.getDecoder().decode(data);
					
					//Now send this off to the DAPPManager.. to be converted into a minidapp..
					MiniData dapp = new MiniData(bdata);
					
					//POST it..
					Message msg = new Message(DAPPManager.DAPP_INSTALL);
					msg.addObject("minidapp", dapp);
					
					mDAPPManager.PostMessage(msg);
				}
				 
				// send HTTP Headers
				out.println("HTTP/1.1 200 OK");
				out.println("Server: HTTP RPC Server from Minima : 1.0");
				out.println("Date: " + new Date());
				out.println("Content-type: "+content);
				out.println("Content-length: " + filelen);
				
				//May turn this off.. for now ok
				out.println("Access-Control-Allow-Origin: *");
				out.println(); // blank line between headers and content, very important !
				
				//Now write the file data
				out.write(file, 0, filelen);
				out.flush(); // flush character output stream buffer
				
			}
			
		} catch (Exception ioe) {
//			ioe.printStackTrace();
//			System.err.println("Server : " +fileRequested+" "+ioe);
			
		} finally {
			try {
				in.close();
				out.close();
				mSocket.close(); // we close socket connection
			} catch (Exception e) {
//				System.err.println("Error closing stream : " + e.getMessage());
			} 	
		}	
	}
	
	
	public String createMiniDAPPList() throws Exception {
		StringBuilder list = new StringBuilder();
		
		JSONArray alldapps = mDAPPManager.getMiniDAPPS();
		
		list.append("<table width=100%>");
		
		int len = alldapps.size();
		for(int i=0;i<len;i++) {
			JSONObject app = (JSONObject) alldapps.get(i);
			
			//Now do it..
			String root  = (String) app.get("root");
			String approot  = (String) app.get("approot");
			String name  = (String) app.get("name");
			String desc  = (String) app.get("description");
			String backg = root+"/"+(String) app.get("background");
			String icon  = root+"/"+(String) app.get("icon");
			String webpage  = root+"/index.html";
			
			//Now do it..
			list.append("<tr><td>" + 
					"			<table style='background-size:100%;background-image: url("+backg+");' width=100% height=100 class=minidapp>" + 
					"			 	<tr>" + 
					"					<td style='cursor:pointer;' rowspan=2 onclick=\"window.open('"+webpage+"', '_blank');\">" + 
					"						<img src='"+icon+"' height=100>" + 
					"					</td>" + 
					"					<td width=100% class='minidappdescription'>" + 
					"                   <div style='position:relative'>" + 
					"				        <div onclick='uninstallDAPP(\""+name+"\",\""+approot+"\");' style='color:red;cursor:pointer;position:absolute;right:10;top:10'>UNINSTALL</div>" + 
					"						<br>" + 
					"						<div onclick=\"window.open('"+webpage+"','_blank');\" style='cursor:pointer;font-size:18'><b>"+name.toUpperCase()+"</b></div>" + 
					"						<br><div onclick=\"window.open('"+webpage+"','_blank');\" style='cursor:pointer;font-size:12'>"+desc+"</div>" + 
					"					</div>"+
					"                     </td>" + 
					"				</tr>" + 
					"			</table>" + 
					"		</td></tr>");
		}
		
		if(len == 0) {
			list.append("<tr><td><br><br>&nbsp;&nbsp;<b>NO DAPPS INSTALLED YET..</b></td></tr>");
		}
		
		list.append("</table>");
		
		return list.toString();
	}
	
	
	
	
	private static final String RESOURCE_BASE = "org/minima/system/network/minidapps/resources/";
	
	public byte[] getResourceBytes(String zResource) throws IOException {
		
		InputStream in = getClass().getClassLoader().getResourceAsStream(RESOURCE_BASE+zResource);
		
		//Doesn't exist..
		if(in == null) {
			return new byte[0];
		}
		
		DataInputStream dis = new DataInputStream(in);
		
		// available stream to be read
        int length = dis.available();
        
        // create buffer
        byte[] buf = new byte[length];
        
        // read the full data into the buffer
        dis.readFully(buf);
        
        return buf;
	}
	
    public byte[] getFileBytes(String zFile) throws IOException {
    	File ff = new File(zFile);
    	
    	long size = ff.length();
    	byte[] ret = new byte[(int) size];
    	
    	try {
			FileInputStream fis     = new FileInputStream(zFile);
			BufferedInputStream bis = new BufferedInputStream(fis);
			
			bis.read(ret);
	        
	        bis.close();
	        fis.close();
	        
		} catch (IOException e) {
			e.printStackTrace();
		} 
        
        return ret;
	}
	
	public static String getContentType(String zEnding) {
		
		if(zEnding.equals("html")) {
			return "text/html";
		}else if(zEnding.equals("htm")) {
			return "text/html";
		}else if(zEnding.equals("css")) {
			return "text/css";
		}else if(zEnding.equals("js")) {
			return "text/javascript";
		}else if(zEnding.equals("txt")) {
			return "text/plain";
		}else if(zEnding.equals("xml")) {
			return "text/xml";
		
		}else if(zEnding.equals("jpg")) {
			return "image/jpeg";
		}else if(zEnding.equals("jpeg")) {
			return "image/jpeg";
		}else if(zEnding.equals("png")) {
			return "image/png";
		}else if(zEnding.equals("gif")) {
			return "image/gif";
		}else if(zEnding.equals("svg")) {
			return "image/svg+xml";
		}else if(zEnding.equals("ico")) {
			return "image/ico";
		
		}else if(zEnding.equals("zip")) {
			return "application/zip";
		}else if(zEnding.equals("pdf")) {
			return "application/pdf";
			
		}else if(zEnding.equals("mp3")) {
			return "audio/mp3";
		}else if(zEnding.equals("wav")) {
			return "audio/wav";
		}
		
		return "text/plain";
	}
	
	public static int indexOf(byte[] outerArray, byte[] smallerArray, int zOffSet) {
		
		for(int i = zOffSet; i < outerArray.length - smallerArray.length+1; ++i) {
	        boolean found = true;
	        for(int j=0; j<smallerArray.length; ++j) {
	           if (outerArray[i+j] != smallerArray[j]) {
	               found = false;
	               break;
	           }
	        }
	        if (found) return i;
	     }
	   return -1;  
	}
	
//	public static void main(String[] zArgs) {
//		
////		byte[] big = new byte[] {1,2,3,0,4,5,6,7,0,8,9,0,0,1,3,4,56};
////		byte[] small = new byte[] {7,0,8,9,0,0,1};
////		System.out.println(indexOf(big, small,0));
//		
//		JSONParser parse = new JSONParser();
//		 try {
//			JSONObject json =  (JSONObject) parse.parse("{\"number\": 10 }");
//			
//			System.out.println(json.toString());
//			
//		} catch (ParseException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}
//		
//	}
}
