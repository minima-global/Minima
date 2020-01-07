package org.minima.system.network.webproxy;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.net.URLDecoder;
import java.util.Date;
import java.util.StringTokenizer;

import org.minima.system.input.InputHandler;
import org.minima.system.input.InputMessage;
import org.minima.utils.MiniFormat;
import org.minima.utils.ResponseStream;
import org.minima.utils.messages.Message;

/**
 * This class handles a single request then exits
 * 
 * @author spartacusrex
 *
 */
public class ProxyRPCHandler implements Runnable {

	/**
	 * The Net Socket
	 */
	Socket mSocket;
	
	/**
	 * Inputhandler to manage the requests
	 */
	MainProxyHandler mHandler;
	
	/**
	 * Main COnstructor
	 * @param zSocket
	 */
	public ProxyRPCHandler(Socket zSocket, MainProxyHandler zHandler) {
		//Store..
		mSocket = zSocket;
		
		//The InputHandler
		mHandler = zHandler;
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
			fileRequested = parse.nextToken().toLowerCase();
			
			// we support only GET and HEAD methods, we check
			if (method.equals("GET")){
				String function=new String(fileRequested);
				if(function.startsWith("/")) {
					function = function.substring(1);
				}
				
				//decode URL message
				function = URLDecoder.decode(function,"UTF-8").trim();
				
				//Break it down..
				if(function.equals("favicon.ico") || function.equals("")) {
					//No good..
					throw new IOException("favion.ico or blank ["+function+"] is nooo good..");
				}
				
				//Break it down..
				int index  = function.indexOf("&");
				String uid = function.substring(0, index);
				String req = function.substring(index+1);
				
				System.out.println("proxy function : "+req);
				System.out.println("proxy uid      : "+uid);
				
				//Now make this request
				ResponseStream response = new ResponseStream();
	            
				//Create a message
				Message proxyreq = new Message(MainProxyHandler.PROXY_RPC_REQUEST);
				proxyreq.addObject("response", response);
				proxyreq.addString("request", req);
				proxyreq.addObject("webid", uid);
				
				//Post it..
				mHandler.PostMessage(proxyreq);
				
				//Wait for the function to finish
                response.waitToFinish();
			
                //Get the response..
                String resp = response.getResponse();
                
                //Check it's a JSON - Hack for now..
                if(resp.startsWith("{")) {
                	resp = MiniFormat.PrettyJSON(resp);
                }
				
				// send HTTP Headers
				out.println("HTTP/1.1 200 OK");
				out.println("Server: HTTP RPC Server from Minima : 1.0");
				out.println("Date: " + new Date());
				out.println("Content-type: text/plain");
				out.println("Content-length: " + resp.length());
				out.println("Access-Control-Allow-Origin: *");
				out.println(); // blank line between headers and content, very important !
				out.println(resp);
				out.flush(); // flush character output stream buffer
			}
			
		} catch (IOException ioe) {
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
	
//	// return supported MIME Types
//	private String getContentType(String fileRequested) {
//		if (fileRequested.endsWith(".htm")  ||  fileRequested.endsWith(".html"))
//			return "text/html";
//		else
//			return "text/plain";
//	}
//	

}
