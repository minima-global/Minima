package org.minima.utils;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URLEncoder;

import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManager;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.json.parser.ParseException;
import org.minima.utils.ssl.MinimaTrustManager;

public class MinimaRPCClient {

	public static void main(String[] zArgs) throws IOException {		
	
		String host 	 = "http://127.0.0.1:9005";
		
		boolean bpass = false;
		
		String username  = "minima";
		String password  = "password";
		String sslpubkey = "";
		
		//One time function
		String command = "";
		
		//Are there any Args
		int arglen 	= zArgs.length;
		if(arglen > 0) {
			int counter	= 0;
			while(counter<arglen) {
				String arg 	= zArgs[counter];
				counter++;
				
				if(arg.equals("-host")) {
					host = zArgs[counter++];
					
				}else if(arg.equals("-username")) {
					username = zArgs[counter++];
				
				}else if(arg.equals("-password")) {
					password = zArgs[counter++];
					bpass = true;
					
				}else if(arg.equals("-sslpubkey")) {
					sslpubkey = zArgs[counter++];
				
				}else if(arg.equals("-command")) {
					command = zArgs[counter++];
					
				}else if(arg.equals("-help")) {
					
					System.out.println("MinimaRPCClient Help");
					System.out.println(" -host       : Specify the host IP:PORT");
					System.out.println(" -password   : Specify the RPC Basic AUTH password (use with SSL)");
					System.out.println(" -username   : Specify the RPC Basic AUTH Username (defaults to minima)");
					System.out.println(" -command    : Specify a single command to run");
					System.out.println(" -sslpubkey  : The SSL public key from Minima rpc command ( if using SSL )");
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
		SSLContext sslcontext = null;
		if(host.startsWith("https://")) {
			ssl = true;
			
			//Create the Trust Manager
			TrustManager[] tm = null;
			if(sslpubkey.equals("")) {
				tm = MinimaTrustManager.getTrustManagers();
			}else {
				tm = MinimaTrustManager.getTrustManagers(new MiniData(sslpubkey));				
			}
			
			//And now the SSL Context
			try {
				sslcontext = SSLContext.getInstance("SSL");
				sslcontext.init(null, tm, new java.security.SecureRandom());
			}catch(Exception exc) {
				MinimaLogger.log(exc);
				System.exit(1);
			}
		}
		
		//make sure host
		if(!host.endsWith("/")) {
			host = host+"/";
		}
		
		//Now lets go..
		if(command.equals("")) {
			MinimaLogger.log("**********************************************");
			MinimaLogger.log("*  __  __  ____  _  _  ____  __  __    __    *");
			MinimaLogger.log("* (  \\/  )(_  _)( \\( )(_  _)(  \\/  )  /__\\   *");
			MinimaLogger.log("*  )    (  _)(_  )  (  _)(_  )    (  /(__)\\  *");
			MinimaLogger.log("* (_/\\/\\_)(____)(_)\\_)(____)(_/\\/\\_)(__)(__) *");
			MinimaLogger.log("*                                            *");
			MinimaLogger.log("**********************************************");
			MinimaLogger.log("Welcome to the Minima RPCClient - for assistance type help. Then press enter.");
			MinimaLogger.log("host        :"+host);
			MinimaLogger.log("ssl         :"+ssl);
			MinimaLogger.log("usepassword :"+bpass);
			MinimaLogger.log("sslpubkey   :"+sslpubkey);
			MinimaLogger.log("To exit this app use 'exit'. 'quit' will shutdown Minima");
		}
		
	    //Loop until finished..
	    String result = null;
	    
	    //One time or multi..
	    if(!command.equals("")) {
	    	
	    	//URLEncode..
        	command = URLEncoder.encode(command, MiniString.MINIMA_CHARSET);
	    	
	    	//Now run this function..
        	if(ssl) {
        		result = RPCClient.sendGETBasicAuthSSL(host+command, username, password, sslcontext);
        	}else{
        		result = RPCClient.sendGETBasicAuth(host+command, username, password);
        	}
			
			//Create a JSON
			JSONObject json;
			try {
				json = (JSONObject) new JSONParser().parse(result);
				
				//Output the result..
				System.out.println(MiniFormat.JSONPretty(json));
				
			} catch (ParseException e) {
				e.printStackTrace();
			}
			
	    }else {
	    	//Listen for input
			InputStreamReader is    = new InputStreamReader(System.in, MiniString.MINIMA_CHARSET);
		    BufferedReader bis      = new BufferedReader(is);
		    
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
		            		result = RPCClient.sendGETBasicAuthSSL(host+input, username, password, sslcontext);
		            	}else{
		            		result = RPCClient.sendGETBasicAuth(host+input,username,password);
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
}
