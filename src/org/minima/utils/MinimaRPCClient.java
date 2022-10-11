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
	
		//Are there any Params..
		String host = "127.0.0.1:9005";
		if(zArgs.length>0) {
			host = zArgs[0];
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
	    			String result = RPCClient.sendGET("http://"+host+"/"+input);
	    			
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
