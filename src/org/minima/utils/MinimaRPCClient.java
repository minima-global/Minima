package org.minima.utils;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URLEncoder;

import org.minima.objects.base.MiniString;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;

public class MinimaRPCClient {

	public static void main(String[] zArgs) throws IOException {		
	
		String host 	= "http://127.0.0.1:9005";
		String password = "";
		
		//Are there any Args
		int arglen 	= zArgs.length;
		if(arglen > 0) {
			int counter	= 0;
			while(counter<arglen) {
				String arg 	= zArgs[counter];
				counter++;
				
				if(arg.equals("-host")) {
					host = zArgs[counter++];
					
				}else if(arg.equals("-password")) {
					password = zArgs[counter++];
					
				}else if(arg.equals("-help")) {
					
					System.out.println("MinimaRPCClient Help");
					System.out.println(" -host       : Specify the host IP:PORT");
					System.out.println(" -password   : Specify the RPC Basic AUTH password (use with SSL)");
					System.out.println(" -help       : Print this help");
					
					System.exit(1);
					
				}else {
					System.out.println("Unknown parameter : "+arg);
					System.exit(1);
				}
			}
		}
		
		//Are we in SSL mode..
		boolean ssl = false;
		if(host.startsWith("https://")) {
			ssl = true;
		}
		
		//make sure host
		if(!host.endsWith("/")) {
			host = host+"/";
		}
		
		//Now lets go..
		MinimaLogger.log("**********************************************");
		MinimaLogger.log("*  __  __  ____  _  _  ____  __  __    __    *");
		MinimaLogger.log("* (  \\/  )(_  _)( \\( )(_  _)(  \\/  )  /__\\   *");
		MinimaLogger.log("*  )    (  _)(_  )  (  _)(_  )    (  /(__)\\  *");
		MinimaLogger.log("* (_/\\/\\_)(____)(_)\\_)(____)(_/\\/\\_)(__)(__) *");
		MinimaLogger.log("*                                            *");
		MinimaLogger.log("**********************************************");
		MinimaLogger.log("Welcome to the Minima RPCClient - for assistance type help. Then press enter.");
		MinimaLogger.log("To 'exit' this app use 'exit'. 'quit' will shutdown Minima");
		
		//Listen for input
		InputStreamReader is    = new InputStreamReader(System.in, MiniString.MINIMA_CHARSET);
	    BufferedReader bis      = new BufferedReader(is);
	    
	    //Loop until finished..
	    String result = null;
	    while(true){
	        try {
	            //Get a line of input
	            String input = bis.readLine();
	            
	            //Check valid..
	            if(input!=null && !input.equals("")) {
	            	//trim it..
	            	input = input.trim();
	            	if(input.equals("exit")) {
	        			break;
	            	}
	            	
	            	//URLEncode..
	            	input = URLEncoder.encode(input, MiniString.MINIMA_CHARSET);
	            	
	            	//Now run this function..
	            	if(ssl) {
	            		result = RPCClient.sendGETBasicAuthSSL(host+input,"minima",password);
	            	}else{
	            		result = RPCClient.sendGETBasicAuth(host+input,"minima",password);
	            	}
	    			
	    			//Create a JSON
	    			JSONObject json = (JSONObject) new JSONParser().parse(result);
	    			
	    			//Output the result..
	    			System.out.println(MiniFormat.JSONPretty(json));
	    			
	            	if(input.equals("quit")) {
	        			break;
	            	}
	            }
	            
	        } catch (Exception ex) {
	            MinimaLogger.log(ex);
	        }
	    }
	    
	    //Cross the streams..
	    try {
	        bis.close();
	        is.close();
	    } catch (IOException ex) {
	    	MinimaLogger.log(""+ex);
	    }
	}
}
