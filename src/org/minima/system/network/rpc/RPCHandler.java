package org.minima.system.network.rpc;

import java.io.BufferedReader;
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
	 * Inputhandler to manage the requests
	 */
	InputHandler mInputHandler;
	
	/**
	 * Main COnstructor
	 * @param zSocket
	 */
	public RPCHandler(Socket zSocket, InputHandler zInput) {
		//Store..
		mSocket = zSocket;
		
		//The InputHandler
		mInputHandler = zInput;
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
			
			//Do we make the output look pretty
			boolean prettyjson = false;
			
			// we support only GET and HEAD methods, we check
			if (method.equals("GET")){
//				System.out.println("fileRequested : "+fileRequested);
				
				String function=new String(fileRequested);
				
				//decode URL message
				function = URLDecoder.decode(function,"UTF-8").trim();
				
				if(function.startsWith("/")) {
					function = function.substring(1);
				}
				
				if(function.startsWith("prettyjson/")) {
					function   = function.substring(11);
					prettyjson = true;
				}
				
				//Is this a multi function..
				boolean multi = false;
				if(function.indexOf(";")!=-1) {
					//It's a multi
					multi = true;
				}
				
				//The final result
				String result = "";
				
				if(!multi) {
					//Now make this request
					ResponseStream response = new ResponseStream();
		            
					//Make sure vbaliue
					if(!function.equals("")) {
						//Send it..
						InputMessage inmsg = new InputMessage(function, response);
	
						//Post it..
						mInputHandler.PostMessage(inmsg);
						
						//Is it quit..
		                if(!input.toLowerCase().equals("quit")) {
		                	//Wait for the function to finish
			                response.waitToFinish();
			            }
					}
					
					//Get the response..
					result = response.getResponse();
					
				}else {
					//A full JSON array of responses
					JSONArray responses = new JSONArray();
					
					//Cycle through each request..	
					StringTokenizer functions = new StringTokenizer(function,";");
					
					boolean allok = true;
					while(allok && functions.hasMoreElements()) {
						String func = functions.nextToken().trim();
					
						//Now make this request
						ResponseStream response = new ResponseStream();
			            
						//Make sure vbaliue
						if(!func.equals("")) {
							//Send it..
							InputMessage inmsg = new InputMessage(func, response);
		
							//Post it..
							mInputHandler.PostMessage(inmsg);
							
							//Is it quit..
			                if(!input.toLowerCase().equals("quit")) {
			                	//Wait for the function to finish
				                response.waitToFinish();
				            }
			                
			                //Get the JSON
			                JSONObject resp = response.getFinalJSON();
			                
			                //IF there is an erorr.. STOP
			                if(resp.get("status") == Boolean.FALSE) {
			                	//ERROR - stop running functions..
			                	allok = false;
			                }
			                
			                //Add it to the array
			                responses.add(resp);
						}
					}
					
					//And now get all the answers in one go..
					result = responses.toString();
				}
				
                //Check it's a JSON
                if(prettyjson) {
					if(result.startsWith("{") || result.startsWith("[")) {
	                	result = MiniFormat.PrettyJSON(result);
	                }
                }
				
				// send HTTP Headers
				out.println("HTTP/1.1 200 OK");
				out.println("Server: HTTP RPC Server from Minima : 1.0");
				out.println("Date: " + new Date());
				out.println("Content-type: text/plain");
				out.println("Content-length: " + result.length());
				out.println("Access-Control-Allow-Origin: *");
				out.println(); // blank line between headers and content, very important !
				out.println(result);
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
