package org.minima.system.network.minidapps;

import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.Date;
import java.util.StringTokenizer;

import org.minima.objects.base.MiniData;
import org.minima.system.input.InputHandler;
import org.minima.system.input.InputMessage;
import org.minima.utils.MiniFormat;
import org.minima.utils.ResponseStream;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

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
	
	/**
	 * Main Constructor
	 * @param zSocket
	 */
	public DAPPHandler(Socket zSocket) {
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
//			while (!input.isEmpty()) { 
//				System.out.println(input); 
//				input = in.readLine(); 
//			}
			
			// we parse the request with a string tokenizer
			StringTokenizer parse = new StringTokenizer(input);
			String method = parse.nextToken().toUpperCase(); // we get the HTTP method of the client
			
			// we get file requested
			fileRequested = parse.nextToken();
			
			// we support only GET and HEAD methods, we check
			if (method.equals("GET")){
				System.out.println("DAPP Request : "+fileRequested);
			
				if(fileRequested.startsWith("/")) {
					fileRequested = fileRequested.substring(1);
				}
				
				//decode URL message
				fileRequested = URLDecoder.decode(fileRequested,"UTF-8").trim();
				
				//The Blank file is index.htm
				if(fileRequested.equals("")) {
					fileRequested = "index.html";
				}
				
				//Get the File..
				byte[] file = getResourceBytes(fileRequested, "text");
				
				//Is it an image or a html file
				if(fileRequested.endsWith(".html")) {
					
				}
				
//				if(file == null) {
//					//ERROR
//					System.out.println("FILE "+fileRequested+" not found..");
//					file = "FILE "+fileRequested+" not found..";
//				}
				
				// send HTTP Headers
				out.println("HTTP/1.1 200 OK");
				out.println("Server: HTTP RPC Server from Minima : 1.0");
				out.println("Date: " + new Date());
				out.println("Content-type: text/html");
//				out.println("Content-length: " + file.length());
//				out.println("Access-Control-Allow-Origin: *");
//				out.println(); // blank line between headers and content, very important !
//				out.println(file);
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
	
	
	private static final String RESOURCE_BASE = "org/minima/system/network/minidapps/resources/";
	
	public byte[] getResourceBytes(String zResource, String zType) throws IOException {
		
		InputStream in = getClass().getClassLoader().getResourceAsStream(RESOURCE_BASE+zResource);
		
		//Doesn't exist..
		if(in == null) {
			return null;
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
	
}
