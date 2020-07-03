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
import org.minima.system.network.minidapps.minilib.Command;
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
//				System.out.println("Header : "+input);
				input = in.readLine();
			}
//			if(!MiniDAPPID.equals("")) {
//				System.out.println("MiniDAPPID:"+MiniDAPPID);	
//			}
			
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
				
				BackupManager backup = InputHandler.getMainInputHandler().getMainHandler().getBackupManager();
				
				//Is this a SQL function
				if(function.startsWith("sql/")) {
					//The SQL results
					JSONObject res = new JSONObject();
					
					//Where is the database..
					File minidappdatabase = null;
					
					//Which Database.. could be running from a folder..
					if(MiniDAPPID.equals("")) {
						//Get the database folder
						File temp = backup.getTempFolder();
						minidappdatabase = new File(temp,"_tempdb"+InputHandler.getMainInputHandler().RANDOM_VAL.to0xString());
						
					}else {
						//Get the database folder
						File minidapps   = backup.getMiniDAPPFolder();
						File dapp        = new File(minidapps,MiniDAPPID);
						
						File dbdir       = new File(dapp,"sql");
						dbdir.mkdirs();
						
						minidappdatabase = new File(dbdir,"_sqldb");
					}
					
					//Get the Function..
					String sql = function.substring(4).trim();
					res.put("db", minidappdatabase.getAbsolutePath());
					res.put("sql", sql);
					
				    //Now lets do some SQL
					try {
						//Start the SQL handler
						SQLHandler handler = new SQLHandler(minidappdatabase.getAbsolutePath());
							
						//Run the SQL..
						if(sql.indexOf(";")!=-1) {
							JSONArray resp  = handler.executeMultiSQL(sql);
							res.put("status", true);
							res.put("response", resp);
						}else {
							JSONObject resp = handler.executeSQL(sql);	
							res.put("status", true);
							res.put("response", resp);
						}
						
						//Close it..
						handler.close();
						
					}catch (SQLException e) {
						res.put("status", false);
						res.put("message", e.toString());
					}
					
					//The response returned..
					finalresult = res.toString();
					
				}else{
					//trim it..
					function = function.trim();

	            	//Create a Command
	            	Command cmd = new Command(function);
	            	
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
			}
			
		} catch (Exception ioe) {
			System.err.println("Server error : " + ioe);
			
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
