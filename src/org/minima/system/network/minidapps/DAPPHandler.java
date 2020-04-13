package org.minima.system.network.minidapps;

import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
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
		PrintStream out 	 			= null; 
		
		String fileRequested 			= null;
		
		try {
			// Input Stream
			in = new BufferedReader(new InputStreamReader(mSocket.getInputStream()));
			
			// Output Stream
//			out = new PrintWriter(mSocket.getOutputStream());
			out = new PrintStream(mSocket.getOutputStream());
			
			// get first line of the request from the client
			String input = in.readLine();
			if(input == null) {
				throw new Exception("ZERO Input Request..");
			}
			String firstline = input;
			
			//Content headers..
//			while (input!=null && !input.isEmpty()) { 
//				System.out.println(input); 
//				input = in.readLine(); 
//			}
			
			// we parse the request with a string tokenizer
			StringTokenizer parse = new StringTokenizer(firstline);
			String method = parse.nextToken().toUpperCase(); // we get the HTTP method of the client
			
			// we get file requested
			fileRequested = parse.nextToken();
			
			// we support only GET and HEAD methods, we check
			if (method.equals("GET")){
				if(fileRequested.endsWith("/")) {
					fileRequested = fileRequested.concat("index.html");
				}
				
				if(fileRequested.startsWith("/")) {
					fileRequested = fileRequested.substring(1);
				}
				
				//decode URL message
				fileRequested = URLDecoder.decode(fileRequested,"UTF-8").trim();
				
				System.out.println("DAPP Request : "+fileRequested);
				
				//Get the File..
				byte[] file = getResourceBytes(fileRequested, "text");
				int filelen = file.length;
				//Found it ?
				if(file == null) {
					//ERROR
					out.println("HTTP/1.1 404 Not Found");
					out.println("Server: HTTP Server from Minima : 1.0");
					out.println("Date: " + new Date());
					out.println("Content-type: text/html");
					out.println("Content-length: 0");
					out.println("Access-Control-Allow-Origin: *");
					out.println(); // blank line between headers and content, very important !
					out.flush(); // flush character output stream buffer
				
				}else {
					
					int dot        = fileRequested.lastIndexOf(".");
					String content = "text/plain";
					if(dot != -1) {
						content = getContentType(fileRequested.substring(dot+1));
					}
					System.out.println("Content "+content);
					 
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
				
			}else {
				System.out.println("NON GET REQUEST ! ");
				
			}
			
		} catch (Exception ioe) {
//			ioe.printStackTrace();
			System.err.println("Server : " +fileRequested+" "+ioe);
			
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
	
	public static String getContentType(String zEnding) {
		if(zEnding.equals("html")) {
			return "text/html";
		}else if(zEnding.equals("htm")) {
			return "text/html";
		}else if(zEnding.equals("css")) {
			return "text/css";
		}else if(zEnding.equals("js")) {
			return "text/javascript";
		}else if(zEnding.equals("jpg")) {
			return "image/jpeg";
		}else if(zEnding.equals("jpeg")) {
			return "image/jpeg";
		}else if(zEnding.equals("png")) {
			return "image/png";
		}else if(zEnding.equals("gif")) {
			return "image/gif";
		}else if(zEnding.equals("pdf")) {
			return "application/pdf";
		}else if(zEnding.equals("txt")) {
			return "text/plain";
		}else if(zEnding.equals("xml")) {
			return "text/xml";
		}else if(zEnding.equals("zip")) {
			return "application/zip";
		}
		
		return "text/plain";
	}
}
