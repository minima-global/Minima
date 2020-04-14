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
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Base64;
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
			InputStreamReader is = new InputStreamReader(mSocket.getInputStream());
			in = new BufferedReader(is);
			
			// Output Stream
			out = new PrintStream(mSocket.getOutputStream());
			
			// get first line of the request from the client
			String input = in.readLine();
			if(input == null) {
				throw new Exception("ZERO Input Request..");
			}
			String firstline = input;
			
			//Content headers..
			int contentlength = 0;
			String boundary = "";
			while (input!=null && !input.isEmpty()) { 
				System.out.println(input);
				
				//Find the Length..
				if(input.indexOf("Content-Length:") != -1) {
					String contlen = input.substring(15);
					System.out.println("LENGTH : "+contlen);
					contentlength = Integer.parseInt(contlen.trim());
				}
				
				//Find the Boundary if required in POST
				int bound = input.indexOf("boundary=");
				if(bound != -1) {
					boundary = input.substring(bound+9);
					System.out.println("BOUNDARY : "+boundary);
				}
				input = in.readLine(); 
			}
			
			// we parse the request with a string tokenizer
			StringTokenizer parse = new StringTokenizer(firstline);
			String method = parse.nextToken().toUpperCase(); // we get the HTTP method of the client
			
			// we get file requested
			fileRequested = parse.nextToken();
			
			if(fileRequested.endsWith("/")) {
				fileRequested = fileRequested.concat("index.html");
			}
			
			if(fileRequested.startsWith("/")) {
				fileRequested = fileRequested.substring(1);
			}
			
			//decode URL message
			fileRequested = URLDecoder.decode(fileRequested,"UTF-8").trim();
			
			//Get the File..
			byte[] file = getResourceBytes(fileRequested, "text");
			int filelen = file.length;
			
			//Did we find it.. ?
			if(filelen == 0) {
				out.println("HTTP/1.1 404 Not Found");
				out.println("Server: HTTP Server from Minima : 1.0");
				out.println("Date: " + new Date());
				out.println("Content-type: text/html");
				out.println("Content-length: 0");
				out.println("Access-Control-Allow-Origin: *");
				out.println(); // blank line between headers and content, very important !
				out.flush(); // flush character output stream buffer
			
			}else {
			
				if (method.equals("GET")){
				
					int dot        = fileRequested.lastIndexOf(".");
					String content = "text/plain";
					if(dot != -1) {
						content = getContentType(fileRequested.substring(dot+1));
					}
					 
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
				
				}else if (method.equals("POST")){
					System.out.println("Readng POST Request Headers"); 
					
					//The data buffer for all the data 
					char[] alldata = new char[contentlength]; 
					in.read(alldata, 0, contentlength);
					
					String complete = new String(alldata);
					
					//Decode..
					String decoded = URLDecoder.decode(complete,"UTF-8");
					System.out.println(decoded);
					
					//Base64 decode..
					int index = decoded.indexOf("base64,");
					
					//Just the Base64 bit
					String data = decoded.substring(index+7);
					
					//Convert to Byte..
					byte[] bdata = Base64.getDecoder().decode(data);
					
					System.out.println("Data len : "+bdata.length);
					
//					input = in.readLine();
//					while(!input.isEmpty()) {
//						System.out.println(input);
//						input = in.readLine();
//					}
//					
//					//Now read the Data
//					input = in.readLine();
//					while(!input.isEmpty()) {
//						System.out.println(input);
//						input = in.readLine();
//					}
//					
//					//Now the finale
//					input = in.readLine();
//					while(!input.isEmpty()) {
//						System.out.println(input);
//						input = in.readLine();
//					}
					
					//Read it ALL in..
//					byte[] strdata = new byte[contentlength];
//					for(int i=0;i<contentlength;i++) {
//						strdata[i] = (byte) (in.read() & 0xFF);
//					}
//					
//					char[] alldata = new char[contentlength]; 
//					
//					//ALL the data..
//					in.read(alldata, 0, contentlength);
//					
////					//Create a String..
////					byte[] strdata = new String(alldata).getBytes(StandardCharsets.UTF_8);
//					
//					ByteBuffer bb  = Charset.forName("UTF-8").encode(CharBuffer.wrap(alldata));
//					byte[] strdata = new byte[bb.remaining()];
//					bb.get(strdata);
//					
////					System.out.println(Arrays.toString(b));
//					
//					System.out.println("STRLEN : "+strdata.length);
//					
//					//START of file.. 
//					byte[] startFile  = new String("\r\n\r\n").getBytes(StandardCharsets.UTF_8);
//					
//					//Now find the the first \r\n
//					int boundstart = indexOf(strdata,startFile,0)+4;
//					
//					//END of file.. 
//					byte[] endFile = new String("\r\n--"+boundary).getBytes(StandardCharsets.UTF_8);
//					
//					//And Now get the END of the file..
//					int boundend = indexOf(strdata,endFile,boundstart);
//					
//					//Now get that Data..
//					int datalen = boundend - boundstart;
//					
//					System.out.println("DATA : "+datalen);
					
					
					int dot        = fileRequested.lastIndexOf(".");
					String content = "text/plain";
					if(dot != -1) {
						content = getContentType(fileRequested.substring(dot+1));
					}
					 
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
				
					
				}else {
					System.out.println("NON GET REQUEST ! ");
				}
			}
			
		} catch (Exception ioe) {
			ioe.printStackTrace();
//			System.err.println("Server : " +fileRequested+" "+ioe);
			
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
			return new byte[0];
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
		}else if(zEnding.equals("txt")) {
			return "text/plain";
		}else if(zEnding.equals("xml")) {
			return "text/xml";
		
			
		}else if(zEnding.equals("jpg")) {
			return "image/jpeg";
		}else if(zEnding.equals("jpeg")) {
			return "image/jpeg";
		}else if(zEnding.equals("png")) {
			return "image/png";
		}else if(zEnding.equals("gif")) {
			return "image/gif";
		}else if(zEnding.equals("ico")) {
			return "image/ico";
		
		
		}else if(zEnding.equals("zip")) {
			return "application/zip";
		}else if(zEnding.equals("pdf")) {
			return "application/pdf";
			
		}else if(zEnding.equals("mp3")) {
			return "audio/mp3";
		}else if(zEnding.equals("wav")) {
			return "audio/wav";
		}
		
		return "text/plain";
	}
	
	public static int indexOf(byte[] outerArray, byte[] smallerArray, int zOffSet) {
		
		for(int i = zOffSet; i < outerArray.length - smallerArray.length+1; ++i) {
	        boolean found = true;
	        for(int j=0; j<smallerArray.length; ++j) {
	           if (outerArray[i+j] != smallerArray[j]) {
	               found = false;
	               break;
	           }
	        }
	        if (found) return i;
	     }
	   return -1;  
	}
	
	public static void main(String[] zArgs) {
		
		byte[] big = new byte[] {1,2,3,0,4,5,6,7,0,8,9,0,0,1,3,4,56};
		byte[] small = new byte[] {7,0,8,9,0,0,1};
		
		System.out.println(indexOf(big, small,0));
		
		
	}
}
