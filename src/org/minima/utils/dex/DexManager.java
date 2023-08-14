package org.minima.utils.dex;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.Socket;
import java.net.URLDecoder;
import java.util.StringTokenizer;

import org.minima.objects.base.MiniString;
import org.minima.utils.MinimaLogger;

public class DexManager implements Runnable {

	Socket mSocket;
	
	public DexManager(Socket zSocket) {
		mSocket = zSocket;
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
			
			MinimaLogger.log(fileRequested,false);
			
//			//Get the command / params only
//			int index 		= fileRequested.indexOf("?");
//			String command 	= fileRequested.substring(0,index);
//			String params 	= fileRequested.substring(index+1);
//			
//			//Get the UID
//			String uid = "";
//			StringTokenizer strtok = new StringTokenizer(params,"&");
//			while(strtok.hasMoreElements()) {
//				String tok = strtok.nextToken();
//				
//				index 			= tok.indexOf("=");
//				String param 	= tok.substring(0,index);
//				String value 	= tok.substring(index+1,tok.length());
//				
//				if(param.equals("uid")) {
//					uid=value;
//				}
//			}
			
//			//Convert ther sessionid
//			String minidappid = mMDS.convertSessionID(uid);
//			if(minidappid == null) {
//				throw new IllegalArgumentException("Invalid session id for MiniDAPP "+uid);
//			}
			
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
			if(!method.equals("POST")) {
				
				//Not a valid request
				throw new IOException("Invalid request not POST");
				
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
		
				MinimaLogger.log("DATA REQUEST : "+data);
			}
		
        } catch (Exception e) {
        	MinimaLogger.log(e);
			
        }finally {
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
