/**
 * 
 */
package org.minima;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;

import org.minima.system.Main;
import org.minima.system.backup.BackupManager;
import org.minima.system.input.InputMessage;
import org.minima.utils.MiniFormat;
import org.minima.utils.MinimaLogger;
import org.minima.utils.ResponseStream;

/**
 * @author Paddy Cerri
 *
 */
public class Start {
	
	/**
	 * A static link to the main server
	 */
	public static Main mMainServer;
	public static Main getServer() {
		return mMainServer;
	}
	
	public String mConfFolder;
	
	/**
	 * Simple constructor for iOS and Android
	 */
	public Start() {}
	
	public void fireStarter(String zConfFolder) {
		mConfFolder = zConfFolder;
		
		//Create a separate thread
		Runnable mainrunner = new Runnable() {
			@Override
			public void run() {
				System.out.println("Minima Started..");
				
				//Start up Variables
				ArrayList<String> vars = new ArrayList<>();
				
				vars.add("-daemon");
//				vars.add("-clean");
//				vars.add("-port");
//				vars.add("9001");
//				vars.add("-connect");
//				vars.add("34.90.172.118");
//				vars.add("9001");
				vars.add("-conf");
				vars.add(mConfFolder);
				
				//And call it..
				main( vars.toArray(new String[0]) );
			}
		};
		
		//Run it..
		Thread mainthread=new Thread(mainrunner);
		mainthread.start();
	}
	
	
	/**
	 * Main Minima Entry point from the command line
	 * 
	 * Use -help for instructions
	 * 
	 * @param zArgs
	 */
	public static void main(String[] zArgs){
		//Check command line inputs
		int arglen 				= zArgs.length;
		int port 				= 9001;
		int rpcport 			= 8999;
		
		boolean connect         = true;
		String connecthost      = "34.90.172.118";
		int connectport         = 9001;
		String mifiProxy 		= "http://mifi.minima.global:9000/";
		String host             = "";
		
		boolean clean           = false;
		boolean genesis 		= false;
		boolean daemon          = false;
		
		//Configuration folder
		File conf = new File(System.getProperty("user.home"),".minima");
		String conffolder = conf.getAbsolutePath();
		
		if(arglen > 0) {
			int counter	=	0;
			while(counter<arglen) {
				String arg 	= zArgs[counter];
				counter++;
				
				if(arg.equals("-port")) {
					//The port
					port= Integer.parseInt(zArgs[counter++]);
				
				}else if(arg.equals("-host")) {
					//Hard code the HOST.. 
					host = zArgs[counter++];
					
				}else if(arg.equals("-rpcport")) {
					//The rpcport
					rpcport= Integer.parseInt(zArgs[counter++]);
				
				}else if(arg.equals("-help")) {
					//Printout HELP!
					MinimaLogger.log("Minima v0.4 Alpha Test Net");
					MinimaLogger.log("        -port [port number]    : Specify port to listen on");
					MinimaLogger.log("        -host [IP]             : Specify the host IP");
					MinimaLogger.log("        -rpcport [port number] : Specify port to listen on for RPC connections");
					MinimaLogger.log("        -conf [folder]         : Specify configuration folder, where data is saved");
					MinimaLogger.log("        -private               : Run a private chain. Don't connect to MainNet. Create a genesis tx-pow. Simulate some users.");
					MinimaLogger.log("        -noconnect             : Don't connect to MainNet. Can then connect to private chains.");
					MinimaLogger.log("        -connect [host] [port] : Don't connect to MainNet. Connect to this node.");
					MinimaLogger.log("        -mifiproxy [host:port] : Use this address for MiFi proxy requests and not the default.");
					MinimaLogger.log("        -clean                 : Wipe user files and chain backup. Start afresh.");
					MinimaLogger.log("        -daemon                : Accepts no input from STDIN. Can run in background process.");
					MinimaLogger.log("        -help                  : Show this help");
					MinimaLogger.log("");
					MinimaLogger.log("With zero params Minima will start and connect to the Main Net.");
					
					return;
				
				}else if(arg.equals("-private")) {
					genesis     = true;
					connect 	= false;
					
				}else if(arg.equals("-noconnect")) {
					connect = false;
				
				}else if(arg.equals("-daemon")) {
					daemon = true;
				
				}else if(arg.equals("-connect")) {
					connect = true;
					connecthost = zArgs[counter++];
					connectport = Integer.parseInt(zArgs[counter++]);
				
				}else if(arg.equals("-mifiproxy")) {
					mifiProxy = zArgs[counter++];
					
				}else if(arg.equals("-clean")) {
					clean = true;
					
				}else if(arg.equals("-conf")) {
					conffolder = zArgs[counter++];
					
				}else {
					MinimaLogger.log("UNKNOWN arg.. : "+arg);
					System.exit(0);
				}
			}
		}
		
		//Do we wipe
		File conffile = new File(conffolder);
		if(clean) {
			BackupManager.deleteConfFolder(conffile);
		}
		
		//Start the main Minima server
		Main rcmainserver = new Main(host,port, rpcport, genesis, conffolder);
		
		//Link it.
		mMainServer = rcmainserver;
		
		//Set the connect properties
		rcmainserver.setAutoConnect(connect);
		rcmainserver.mAutoHost = connecthost;
		rcmainserver.mAutoPort = connectport;
		
		//Set the proxy
		rcmainserver.setMiFiProxy(mifiProxy);
		
		//Are we private!
		if(genesis) {
			rcmainserver.privateChain(clean);
		}
		
		//Start the system
		rcmainserver.PostMessage(Main.SYSTEM_STARTUP);
		
		//Are we a daemon thread
		if(daemon) {
			MinimaLogger.log("Daemon Started..");
			
			//Loop while running..
			while (rcmainserver.isRunning()) {
                try {
                    Thread.sleep(1000);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
		}else {
			//Listen for input
		    InputStreamReader is    = new InputStreamReader(System.in);
		    BufferedReader bis      = new BufferedReader(is);
	
		    //Loop until finished..
		    while(rcmainserver.isRunning()){
		        try {
		            //Get a line of input
		            String input = bis.readLine();
		            
		            //Check valid..
		            if(input!=null && !input.equals("")) {
		            	//trim it..
		            	input = input.trim();
		            	
		            	//New response packet..
			            ResponseStream response = new ResponseStream();
			            
		            	//Set the output stream
			            InputMessage inmsg = new InputMessage(input, response);
			            
		            	//Tell main server
		                rcmainserver.getInputHandler().PostMessage(inmsg);
		            
		                //Wait for the function to finish
		                response.waitToFinish();
		                
		                //Get the response..
		                String resp = response.getResponse();
		                
		                //Makethe JSON pretty
		                resp = MiniFormat.JSONPretty(resp);
		                		
		                //And then print out the result
		                System.out.println(resp);
		                
		                //Is it quit..
		                if(input.toLowerCase().equals("quit")) {
			            	break;
			            }
		            }
		            
		        } catch (IOException ex) {
		            MinimaLogger.log(""+ex);
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
		
		MinimaLogger.log("Main thread finished..");
	}
}	

