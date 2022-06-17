package org.minima.system.mds.sql;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.Socket;
import java.net.URLDecoder;
import java.util.Date;
import java.util.StringTokenizer;

import org.minima.objects.base.MiniString;
import org.minima.system.mds.MDSManager;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;

/**
 * This class handles a single request then exits
 * 
 * @author spartacusrex
 *
 */
public class SQLHandler implements Runnable {

	/**
	 * The Net Socket
	 */
	Socket mSocket;
	
	/**
	 * The MDS Manager
	 */
	MDSManager mMDS;
	
	/**
	 * Main Constructor
	 * @param zSocket
	 */
	public SQLHandler(Socket zSocket, MDSManager zMDS) {
		//Store..
		mSocket = zSocket;
		mMDS	= zMDS;
	}

	@Override
	public void run() {
		// we manage our particular client connection
		BufferedReader in 	 		 	= null; 
		PrintWriter out 	 			= null; 
		String firstline = "no first line..";
		
		try {
			// Input Stream
			in = new BufferedReader(new InputStreamReader(mSocket.getInputStream(), MiniString.MINIMA_CHARSET));
			
			// Output Stream
			out = new PrintWriter(new OutputStreamWriter(mSocket.getOutputStream(), MiniString.MINIMA_CHARSET));
			
			// get first line of the request from the client
			String input = in.readLine();
			if (input == null){
				input = "";
			}
			
			//Get the first line..
			firstline = new String(input);
			
			// we parse the request with a string tokenizer
			StringTokenizer parse = new StringTokenizer(input);
			String method = parse.nextToken().toUpperCase(); // we get the HTTP method of the client
			
			// we get file requested
			String fileRequested = parse.nextToken();
			
			//Remove slashes..
			if(fileRequested.startsWith("/")) {
				fileRequested = fileRequested.substring(1);
			}
			if(fileRequested.endsWith("/")) {
				fileRequested = fileRequested.substring(0,fileRequested.length()-1);
			}
			
			//And finally URL decode..
			fileRequested = URLDecoder.decode(fileRequested,"UTF-8").trim();
			
			//Get the UID
			String uid = "";
			StringTokenizer strtok = new StringTokenizer(fileRequested,"&");
			while(strtok.hasMoreElements()) {
				String tok = strtok.nextToken();
				
				int index 		= tok.indexOf("=");
				String param 	= tok.substring(0,index);
				String value 	= tok.substring(index+1,tok.length());
				
				if(param.equals("uid")) {
					uid=value;
				}
			}
			
			//Get the Headers..
			int contentlength = 0;
			while(input != null && !input.trim().equals("")) {
				//MinimaLogger.log("RPC : "+input);
				int ref = input.indexOf("Content-Length:"); 
				if(ref != -1) {
					//Get it..
					int start     = input.indexOf(":");
					contentlength = Integer.parseInt(input.substring(start+1).trim());
				}	
				input = in.readLine();
			}
			
			//Is it a POST request
			if(!method.equals("POST") || uid.equals("")) {
				
				// send HTTP Headers
				out.println("HTTP/1.1 500 OK");
				out.println("Server: HTTP RPC Server from Minima : 1.3");
				out.println("Date: " + new Date());
				out.println("Content-type: text/plain");
				out.println("Access-Control-Allow-Origin: *");
				out.println(); // blank line between headers and content, very important !
				out.flush(); // flush character output stream buffer
				
			}else {
				
				//How much data
				char[] cbuf 	= new char[contentlength];
				
				//Read it all in
				//Read it ALL in
				int len,total=0;
				while( (len = in.read(cbuf,total,contentlength-total)) != -1) {
					total += len;
					if(total == contentlength) {
						break;
					}
				}
				
				if(total != contentlength) {
					MinimaLogger.log("SQLHANDLER : Read wrong amount "+len+"/"+contentlength);	
				}
				
				//Set this..
				String data = new String(cbuf);
				
				//Run the SQL
				JSONObject sqlresult = mMDS.runSQL(uid, data);
				String result 		 = sqlresult.toString(); 
		    	
				//Calculate the size of the response
				int finallength = result.getBytes(MiniString.MINIMA_CHARSET).length;
				
				// send HTTP Headers
				out.println("HTTP/1.1 200 OK");
				out.println("Server: HTTP SQL Server from Minima : 1.3");
				out.println("Date: " + new Date());
				out.println("Content-type: text/plain");
				out.println("Content-length: " + finallength);
				out.println("Access-Control-Allow-Origin: *");
				out.println(); // blank line between headers and content, very important !
				out.println(result);
				out.flush(); // flush character output stream buffer
			}
			
		} catch (Exception ioe) {
			MinimaLogger.log(ioe);
			
		} finally {
			try {
				in.close();
				out.close();
				mSocket.close(); // we close socket connection
			} catch (Exception e) {
				MinimaLogger.log(e);
			} 	
		}	
	}
}
