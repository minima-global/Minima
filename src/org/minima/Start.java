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
import org.minima.utils.messages.Message;

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
				vars.add("-clean");
				vars.add("-port");
				vars.add("9001");
				vars.add("-connect");
				vars.add("34.90.172.118");
				vars.add("9001");
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
		
		//Currently DISABLED .. will re-enable later
		//Is a function called when there is a new relevant transaction..
		//This function could put the data in a web database etc..
		String txnfunction = "";
		String relcoin     = "";
		
		boolean connect         = true;
		String connecthost      = "34.90.172.118";
		int connectport         = 9001;
		String mifiProxy 		= "http://mifi.minima.global:9000/";
		
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
					
				}else if(arg.equals("-rpcport")) {
					//The rpcport
					rpcport= Integer.parseInt(zArgs[counter++]);
				
				}else if(arg.equals("-help")) {
					//Printout HELP!
					MinimaLogger.log("Minima v0.4 Alpha Test Net");
					MinimaLogger.log("        -port [port number]    : Specify port to listen on");
					MinimaLogger.log("        -rpcport [port number] : Specify port to listen on for RPC connections");
					MinimaLogger.log("        -conf [folder]         : Specify configuration folder, where data is saved");
					MinimaLogger.log("        -private               : Run a private chain. Don't connect to MainNet. Create a genesis tx-pow. Simulate some users.");
					MinimaLogger.log("        -noconnect             : Don't connect to MainNet. Can then connect to private chains.");
					MinimaLogger.log("        -connect [host] [port] : Don't connect to MainNet. Connect to this node.");
					MinimaLogger.log("        -mifiproxy [host:port] : Use this address for MiFi proxy requests and not the default.");
//					SimpleLogger.log("        -relcoin [POST_URL]    : HTTP POST of new coins in json format (all in 'data') that are relevant to this wallet.");
					MinimaLogger.log("        -clean                 : Wipe user files and chain backup. Start afresh.");
					MinimaLogger.log("        -daemon                : Accepts no input from STDIN. Can run in background process.");
					MinimaLogger.log("        -help                  : Show this help");
					MinimaLogger.log("");
					MinimaLogger.log("With zero params Minima will start and connect to the Main Net.");
					
					return;
				
				}else if(arg.equals("-private")) {
					genesis     = true;
					connect 	= false;
					clean       = true;
					
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
				
				}else if(arg.equals("-txncall")) {
					txnfunction = zArgs[counter++];
					
				}else if(arg.equals("-relcoin")) {
					relcoin = zArgs[counter++];
					
				}else if(arg.equals("-conf")) {
					conffolder = zArgs[counter++];
					
				}else {
					MinimaLogger.log("UNKNOWN arg.. : "+arg);
					System.exit(0);
				}
			}
		}
		
		//Do we wipe
		if(clean) {
			//BackupManager.deleteFolder(new File(conffolder));
			BackupManager.deleteAllButMiniDAPPS(new File(conffolder));
		}
		
		//Start the main Minima server
		Main rcmainserver = new Main(port, rpcport, genesis, conffolder);
		
		//Link it.
		mMainServer = rcmainserver;
		
		//Set the connect properties
		rcmainserver.setAutoConnect(connect);
		rcmainserver.mAutoHost = connecthost;
		rcmainserver.mAutoPort = connectport;
		
		//Set the proxy
		rcmainserver.setMiFiProxy(mifiProxy);
		
		if(!txnfunction.equals("")) {
			MinimaLogger.log("New Txn function : "+txnfunction);
			rcmainserver.setNewTxnCommand(txnfunction);
		}
		
		if(!relcoin.equals("")) {
			MinimaLogger.log("New Relevant Coin URL : "+relcoin);
			rcmainserver.setNewRelCoin(relcoin);
		}
		
		//Start the system
		rcmainserver.PostMessage(Main.SYSTEM_STARTUP);
		
		rcmainserver.getConsensusHandler().addListener(new NativeListener() {
			@Override
			public void processMessage(Message zMessage) {
				//THIS GETS CALLED!
			}
		});
		
		//Are we a daemon thread
		if(daemon) {
			System.out.println("Daemon Started..");
			
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
		            
		                //Is it quit..
		                if(input.toLowerCase().equals("quit")) {
			            	break;
			            }
		            
		                //Wait for the function to finish
		                response.waitToFinish();
		                
		                //Get the response..
		                String resp = response.getResponse();
		                
		                //Check it's a JSON - Hack for now..
		                if(resp.startsWith("{") || resp.startsWith("[")) {
		                	resp = MiniFormat.PrettyJSON(resp);
		                }
		                
		                //Convert \n..
		                resp = resp.replaceAll("\\\\n", "\n");
		                		
		                //And then print out the result
		                System.out.println(resp);
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
		
	}
}	

