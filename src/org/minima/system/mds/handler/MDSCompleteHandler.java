package org.minima.system.mds.handler;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.Socket;
import java.net.SocketException;
import java.net.URLDecoder;
import java.util.Date;
import java.util.StringTokenizer;

import javax.net.ssl.SSLException;
import javax.net.ssl.SSLHandshakeException;

import org.minima.database.minidapps.MiniDAPP;
import org.minima.objects.base.MiniString;
import org.minima.system.mds.MDSManager;
import org.minima.system.mds.polling.PollStack;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;

/**
 * This class handles a single request then exits
 * 
 * @author spartacusrex
 *
 */
public class MDSCompleteHandler implements Runnable {

	/**
	 * The Net Socket
	 */
	Socket mSocket;
	
	/**
	 * The MDS Manager
	 */
	MDSManager mMDS;
	
	/**
	 * The POLL Stack
	 */
	PollStack mPollStack;
	
	/**
	 * Main Constructor
	 * @param zSocket
	 */
	public MDSCompleteHandler(Socket zSocket, MDSManager zMDS, PollStack zPollStack) {
		//Store..
		mSocket 	= zSocket;
		mMDS		= zMDS;
		mPollStack	= zPollStack;
	}

	@Override
	public void run() {
		// we manage our particular client connection
		BufferedReader in 	 		 	= null; 
		PrintWriter out 	 			= null; 
		
		try {
			// Input Stream
			in = new BufferedReader(new InputStreamReader(mSocket.getInputStream(), MiniString.MINIMA_CHARSET));
			
			// Output Stream
			out = new PrintWriter(new OutputStreamWriter(mSocket.getOutputStream(), MiniString.MINIMA_CHARSET));
			
			// get first line of the request from the client
			String input = in.readLine();
			int counter = 0;
			while(input == null && counter<100){
				//Wait a sec
				Thread.sleep(1000);
				
				input = in.readLine();
				counter++;
			}
			
			//Is it still NULL
			if(input == null) {
				throw new IllegalArgumentException("Invalid NULL MDS request ");
			}
			
			// we parse the request with a string tokenizer
			StringTokenizer parse = new StringTokenizer(input);
			
			//Get the METHOD
			String method = parse.nextToken().toUpperCase(); // we get the HTTP method of the client
			
			//Get the requested file
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
				}
			}
			
			//Convert ther sessionid
			String minidappid = mMDS.convertSessionID(uid);
			if(minidappid == null) {
				throw new IllegalArgumentException("Invalid session id for MiniDAPP "+uid);
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
				
				//Not a valid request
				throw new Exception("Invalid request no UID or not POST");
				
			}else{
				
				//How much data
				char[] cbuf 	= new char[contentlength];
				
				//Read it ALL in
				int len,total=0;
				while( (len = in.read(cbuf,total,contentlength-total)) != -1) {
					total += len;
					if(total == contentlength) {
						break;
					}
				}
				
				//Set this..
				String dataenc 	= new String(cbuf).trim();
				String data 	= URLDecoder.decode(dataenc, "UTF-8");
				
				String result = null;
				if(command.equals("sql")) {
				
					SQLcommand sql = new SQLcommand(mMDS);
					result = sql.runCommand(minidappid, data);
					
				}else if(command.equals("cmd")) {
					
					//Create a Command and run it..
					CMDcommand cmd = new CMDcommand(minidappid, data);
					result = cmd.runCommand();

				}else if(command.equals("notify")) {
					
					//Get the MiniDAPP
					MiniDAPP md = mMDS.getMiniDAPP(minidappid);
					String name = md.getName(); 
					
					//Create a Command and run it..
					NOTIFYcommand notify = new NOTIFYcommand(minidappid, name, data, true);
					result = notify.runCommand();

				}else if(command.equals("notifycancel")) {
					
					//Create a Command and run it..
					NOTIFYcommand notify = new NOTIFYcommand(minidappid, "", "", false);
					result = notify.runCommand();
					
				}else if(command.equals("net")) {
					
					//Create a Command and run it..
					NETcommand net 	= new NETcommand(minidappid, data);
					result 			= net.runCommand();
				
				}else if(command.equals("netpost")) {
					
					//Get the URL and the post data..
					int dataindex 	= data.indexOf("&");
					String url 		= data.substring(0, dataindex);
					String postdata = data.substring(dataindex+1);
					
					//Create a Command and run it..
					NETcommand net 	= new NETcommand(minidappid,url, postdata);
					result 			= net.runCommand();
				
				}else if(command.equals("file")) {
					
					//Get the URL and the post data..
					int dataindex 	= data.indexOf("&");
					String action 	= data.substring(0, dataindex);
					String filedata = data.substring(dataindex+1);
					
					//What was the data
					FILEcommand fc = null;
					if(action.equals("list")) {
						fc = new FILEcommand(mMDS, minidappid, FILEcommand.FILECOMMAND_LIST, 
								filedata, "");
					
					}else if(action.equals("save")) {
						dataindex 			= filedata.indexOf("&");
						String file 		= filedata.substring(0, dataindex);
						String actualedata 	= filedata.substring(dataindex+1);
						fc = new FILEcommand(mMDS, minidappid, FILEcommand.FILECOMMAND_SAVE, 
								file, actualedata);
					
					}else if(action.equals("load")) {
						fc = new FILEcommand(mMDS, minidappid, FILEcommand.FILECOMMAND_LOAD, 
								filedata, "");
					
					}else if(action.equals("delete")) {
						fc = new FILEcommand(mMDS, minidappid, FILEcommand.FILECOMMAND_DELETE, 
								filedata, "");
					
					}else {
						throw new IllegalArgumentException("Invalid function");
					}
					
					//Create a Command and run it..
					result = fc.runCommand();
				
				}else if(command.equals("comms")) {
					
					//Is it public or private
					int dataindex 	= data.indexOf("&");
					String pubpriv 	= data.substring(0, dataindex);
					String msg 		= data.substring(dataindex+1);
					
					//Get the Name of the MiniDAPP..
					MiniDAPP md = mMDS.getMiniDAPP(minidappid);
					
					//Create a Command and run it..
					if(pubpriv.equals("public")) {
						COMMSCommand comms = new COMMSCommand(mMDS, "*", md.getName(),  msg);
						result = comms.runCommand();
					}else {
						COMMSCommand comms = new COMMSCommand(mMDS, minidappid, md.getName(), msg);
						result = comms.runCommand();
					}
					
				}else if(command.equals("poll")) {
					
					POLLcommand poll = new POLLcommand(mPollStack);
					result = poll.runCommand(minidappid, data);
					
				}else{
					
					//Is it a CMD / SQL / FILE / FUNC ..
					MinimaLogger.log("ERROR COMPLETE FILE REQ : "+command+" "+params);
					
					//Invalid command
					JSONObject error = new JSONObject();
					error.put("status", false);
					
					result = error.toString(); 
				}
				
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
			
		}catch(SSLHandshakeException exc) {
		}catch(SSLException exc) {
		
		}catch(SocketException exc) {
			
			// send HTTP Headers
			out.println("HTTP/1.1 500 OK");
			out.println("Server: HTTP MDS Server from Minima : 1.3");
			out.println("Date: " + new Date());
			out.println("Content-type: text/plain");
			out.println("Access-Control-Allow-Origin: *");
			out.println(); // blank line between headers and content, very important !
			out.flush(); // flush character output stream buffer
		
		}catch(IllegalArgumentException exc) {
			MinimaLogger.log(exc.toString());
			
			// send HTTP Headers
			out.println("HTTP/1.1 500 OK");
			out.println("Server: HTTP MDS Server from Minima : 1.3");
			out.println("Date: " + new Date());
			out.println("Content-type: text/plain");
			out.println("Access-Control-Allow-Origin: *");
			out.println(); // blank line between headers and content, very important !
			out.flush(); // flush character output stream buffer
			
		}catch(Exception ioe) {
			MinimaLogger.log(ioe);
			
			// send HTTP Headers
			out.println("HTTP/1.1 500 OK");
			out.println("Server: HTTP MDS Server from Minima : 1.3");
			out.println("Date: " + new Date());
			out.println("Content-type: text/plain");
			out.println("Access-Control-Allow-Origin: *");
			out.println(); // blank line between headers and content, very important !
			out.flush(); // flush character output stream buffer
			
			
		} finally {
			try {
				in.close();
				out.close();
				mSocket.close(); // we close socket connection
			} catch (Exception e) {
//				MinimaLogger.log(e);
			} 	
		}	
	}
}
