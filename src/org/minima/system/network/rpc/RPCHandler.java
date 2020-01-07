package org.minima.system.network.rpc;

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
			fileRequested = parse.nextToken().toLowerCase();
			
			// we support only GET and HEAD methods, we check
			if (method.equals("GET")){
//				System.out.println("fileRequested : "+fileRequested);
			
				String function=new String(fileRequested);
				if(function.startsWith("/")) {
					function = function.substring(1);
				}
				
				//decode URL message
				function = URLDecoder.decode(function,"UTF-8").trim();
				
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
                String resp = response.getResponse();
                
                //Check it's a JSON
                if(resp.startsWith("{") || resp.startsWith("[")) {
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
//				out.flush(); // flush character output stream buffer
				
				out.println(resp);
				out.flush(); // flush character output stream buffer
			}
			
//		} catch (FileNotFoundException fnfe) {
//			try {
//				fileNotFound(out, dataOut, fileRequested);
//			} catch (IOException ioe) {
//				System.err.println("Error with file not found exception : " + ioe.getMessage());
//			}
			
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
			
//			if (verbose) {
//				System.out.println("Connection closed.\n");
//			}
		}
		
		
	}
	
//	private byte[] readFileData(File file, int fileLength) throws IOException {
//		FileInputStream fileIn = null;
//		byte[] fileData = new byte[fileLength];
//		
//		try {
//			fileIn = new FileInputStream(file);
//			fileIn.read(fileData);
//		} finally {
//			if (fileIn != null) 
//				fileIn.close();
//		}
//		
//		return fileData;
//	}
//	
//	// return supported MIME Types
//	private String getContentType(String fileRequested) {
//		if (fileRequested.endsWith(".htm")  ||  fileRequested.endsWith(".html"))
//			return "text/html";
//		else
//			return "text/plain";
//	}
//	
//	private void fileNotFound(PrintWriter out, OutputStream dataOut, String fileRequested) throws IOException {
//		File file = new File(WEB_ROOT, FILE_NOT_FOUND);
//		int fileLength = (int) file.length();
//		String content = "text/html";
//		byte[] fileData = readFileData(file, fileLength);
//		
//		out.println("HTTP/1.1 404 File Not Found");
//		out.println("Server: Java HTTP Server from SSaurel : 1.0");
//		out.println("Date: " + new Date());
//		out.println("Content-type: " + content);
//		out.println("Content-length: " + fileLength);
//		out.println(); // blank line between headers and content, very important !
//		out.flush(); // flush character output stream buffer
//		
//		dataOut.write(fileData, 0, fileLength);
//		dataOut.flush();
//		
//		if (verbose) {
//			System.out.println("File " + fileRequested + " not found");
//		}
//	}
}
