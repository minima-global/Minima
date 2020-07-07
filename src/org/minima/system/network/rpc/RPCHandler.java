package org.minima.system.network.rpc;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.net.URLDecoder;
import java.sql.SQLException;
import java.util.Date;
import java.util.StringTokenizer;

import org.minima.system.backup.BackupManager;
import org.minima.system.input.InputHandler;
import org.minima.system.network.commands.CMD;
import org.minima.system.network.commands.SQL;
import org.minima.utils.MinimaLogger;
import org.minima.utils.SQLHandler;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

/**
 * This class handles a single request then exits
 * 
 * @author spartacusrex
 *
 */
public class RPCHandler implements Runnable {

	/**
	 * The Net Socket
	 */
	Socket mSocket;
	
	/**
	 * Main COnstructor
	 * @param zSocket
	 */
	public RPCHandler(Socket zSocket) {
		//Store..
		mSocket = zSocket;
	}

	@Override
	public void run() {
		// we manage our particular client connection
		BufferedReader in 	 		 	= null; 
		PrintWriter out 	 			= null; 
		
		String fileRequested 			= null;
		
		try {
			// Input Stream
			in = new BufferedReader(new InputStreamReader(mSocket.getInputStream()));
			
			// Output Stream
			out = new PrintWriter(mSocket.getOutputStream());
			
			// get first line of the request from the client
			String input = in.readLine();
			
			// we parse the request with a string tokenizer
			StringTokenizer parse = new StringTokenizer(input);
			String method = parse.nextToken().toUpperCase(); // we get the HTTP method of the client
			
			// we get file requested
			fileRequested = parse.nextToken();
			
			//Get the Headers..
			String MiniDAPPID = "";
			while(input != null && !input.trim().equals("")) {
				int ref = input.indexOf("Referer:"); 
				if(ref != -1) {
					//Get the referer..
					int start  = input.indexOf("/minidapps/0x")+11;
	        		int end    = -1;
	        		if(start!=-1) {
	        			end    = input.indexOf("/", start);
	        		}
	        		if(end!=-1) {
	        			MiniDAPPID = input.substring(start, end);
	        		}
				}
				input = in.readLine();
			}
			
			// Currently we support only GET
			if (method.equals("GET")){
//				System.out.println("fileRequested : "+fileRequested);
				//decode URL message
				String function = URLDecoder.decode(fileRequested,"UTF-8").trim();
				if(function.startsWith("/")) {
					function = function.substring(1);
				}
				
				//The final result
				String finalresult = "";
				
				//Is this a SQL function
				if(function.startsWith("sql/")) {
					//Get the SQL function
					function = function.substring(4).trim();
					
					//Create a SQL object
					SQL sql = new SQL(function, MiniDAPPID);
					
					//Run it..
					sql.run();
					
					//Get the Response..
	            	finalresult = sql.getFinalResult();
					
				}else{
					CMD cmd = new CMD(function.trim());
	            	
	            	//Run it..
	            	cmd.run();
	 
	            	//Get the Response..
	            	finalresult = cmd.getFinalResult();
				}
				
				// send HTTP Headers
				out.println("HTTP/1.1 200 OK");
				out.println("Server: HTTP RPC Server from Minima : 1.0");
				out.println("Date: " + new Date());
				out.println("Content-type: text/plain");
				out.println("Content-length: " + finalresult.length());
				out.println("Access-Control-Allow-Origin: *");
				out.println(); // blank line between headers and content, very important !
				out.println(finalresult);
				out.flush(); // flush character output stream buffer
			
			}else {
				MinimaLogger.log("Unsupported Method in RPCHandler : "+method);
			}
			
		} catch (Exception ioe) {
			ioe.printStackTrace();
			
		} finally {
			try {
				in.close();
				out.close();
				mSocket.close(); // we close socket connection
			} catch (Exception e) {
				System.err.println("Error closing stream : " + e.getMessage());
			} 	
		}	
	}
}
