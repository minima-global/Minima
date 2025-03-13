package org.minima.system.network.rpc;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.Socket;
import java.net.URLDecoder;
import java.util.Date;
import java.util.StringTokenizer;

import org.minima.database.MinimaDB;
import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.commands.CommandRunner;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

/**
 * This class handles a single request then exits
 * 
 * @author spartacusrex
 *
 */
public class CMDHandler implements Runnable {

	/**
	 * The Net Socket
	 */
	Socket mSocket;
	
	/**
	 * Main Constructor
	 * @param zSocket
	 */
	public CMDHandler(Socket zSocket) {
		//Store..
		mSocket = zSocket;
	}

	@Override
	public void run() {
		// we manage our particular client connection
		BufferedReader in 	 		 	= null; 
		PrintWriter out 	 			= null; 
		String firstline 				= "no first line..";
		boolean quit 					= false;
		
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
			
			//Are we Authorizing
			JSONObject authuser = new JSONObject();
			authuser.put("valid", true);
			authuser.put("mode", "write");
			
			//MUST have correct details..
			if(GeneralParams.RPC_AUTHENTICATE || MinimaDB.getDB().getUserDB().getRPCUsers().size()>0) {
				authuser.put("valid", false);
			}
			
			//Get the Headers..
			int contentlength = 0;
			while(input != null && !input.trim().equals("")) {
				//MinimaLogger.log("RPC HEADER : "+input);
				
				//Check if Authorised
				int authref = input.indexOf("Authorization:");
				if(authref != -1) {
					
					//Check it..
					authuser = Authorizer.checkAuchCredentials(input);
				}
				
				int ref = input.indexOf("Content-Length:"); 
				if(ref != -1) {
					//Get it..
					int start     = input.indexOf(":");
					contentlength = Integer.parseInt(input.substring(start+1).trim());
				}	
				input = in.readLine();
			}
			
			//Are we Authorised
			if(!authuser.getBoolean("valid")) {
				
				//Not allowed..
				out.println("HTTP/1.1 401 Unauthorized");
				out.println("Server: HTTP RPC Server from Minima 1.3");
				out.println();
				out.flush(); // flush character output stream buffer
				
				throw new IllegalArgumentException("Invalid Authentication at RPC");
			}
			
			//Is it a POST request
			if(method.equals("POST")) {
				//Get the POST data..
				char[] cbuf = new char[contentlength];
				
				//Read it ALL in
				int len,total=0;
				while( (len = in.read(cbuf,total,contentlength-total)) != -1) {
					total += len;
					if(total == contentlength) {
						break;
					}
				}
				
				if(total != contentlength) {
					MinimaLogger.log("CMDHANDLER : Read wrong amount "+len+"/"+contentlength);	
				}
				
				//Set this..
				fileRequested = new String(cbuf);
			}
			
			JSONObject statfalse = new JSONObject();
			statfalse.put("status", false);
			String result = statfalse.toJSONString();
			try {
				if(fileRequested.equals("quit")) {
					quit=true;
				}
				
				//MinimaLogger.log("RPC:"+fileRequested+" User:"+authuser.toString());
				
				//Now run this function..
				JSONArray res = null;
				if(authuser.getString("mode").equals("read")) {
					res = CommandRunner.getRunner().runMultiCommand(Main.getInstance().getMDSManager().getUntrustedMiniDAPPID(), fileRequested);
				}else if(authuser.getString("mode").equals("write")) {
					res = CommandRunner.getRunner().runMultiCommand(fileRequested);
				}else {
					throw new IllegalArgumentException("Invalid mode for RPC user : "+authuser.getString("mode"));
				}
				
				//Get the result.. is it a multi command or single.. 
				if(res.size() == 1) {
					result = res.get(0).toString();
				}else {
					result = res.toString();
				}
				
//				MinimaLogger.log("RPC RESULT : "+result);
				
			}catch(Exception exc) {
				MinimaLogger.log("ERROR CMDHANDLER : "+fileRequested+" "+exc);
				
			}
			
			//Calculate the size of the response
			int finallength = result.getBytes(MiniString.MINIMA_CHARSET).length; 
			
			//Are we using CRLF
			if(GeneralParams.RPC_CRLF) {
				
				// send HTTP Headers
				out.println("HTTP/1.1 200 OK\r");
				out.println("Server: HTTP RPC Server from Minima 1.3\r");
				out.println("Date: " + new Date()+"\r");
				out.println("Content-type: text/plain\r");
				out.println("Content-length: " + finallength+"\r");
				out.println("Access-Control-Allow-Origin: *\r");
				out.println("\r"); // blank line between headers and content, very important !
				out.println(result);
				out.flush(); // flush character output stream buffer
				
			}else {
				
				// send HTTP Headers
				out.println("HTTP/1.1 200 OK");
				out.println("Server: HTTP RPC Server from Minima 1.3");
				out.println("Date: " + new Date());
				out.println("Content-type: text/plain");
				out.println("Content-length: " + finallength);
				out.println("Access-Control-Allow-Origin: *");
				out.println(); // blank line between headers and content, very important !
				out.println(result);
				out.flush(); // flush character output stream buffer
			}
			
		} catch (Exception ioe) {
			MinimaLogger.log("CMDHANDLER : "+ioe+" "+firstline);
			//MinimaLogger.log(ioe);
			
		} finally {
			try {
				in.close();
				out.close();
				mSocket.close(); // we close socket connection
			} catch (Exception e) {
				MinimaLogger.log(e);
			} 	
		}	
		
		//Are we shutting down
		if(quit) {
			Runtime.getRuntime().halt(0);
		}
	}
}
