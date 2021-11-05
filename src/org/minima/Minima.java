package org.minima;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Iterator;

import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.params.GeneralParams;
import org.minima.system.params.GlobalParams;
import org.minima.system.params.TestParams;
import org.minima.utils.MiniFormat;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageListener;

public class Minima {

	public Minima() {}
	
	public void fireStarter(final String zConfFolder, final String zConnectHost) {
		
		//Create a separate thread
		Runnable mainrunner = new Runnable() {
			@Override
			public void run() {
				System.out.println("Minima Started..");
				
				//Start up Variables
				ArrayList<String> vars = new ArrayList<>();
				
				vars.add("-daemon");
				
//				vars.add("-genesis");
//				vars.add("-test");
				
				vars.add("-connect");
				vars.add(zConnectHost);
				
//				vars.add("-port");
//				vars.add("9001");
				
				vars.add("-conf");
				vars.add(zConfFolder);
				
				//And call it..
				main( vars.toArray(new String[0]) );
			}
		};
		
		//Run it..
		Thread mainthread=new Thread(mainrunner);
		mainthread.start();
	}
	
	/**
	 * Run a command on Minima and return the result
	 */
	public String runMinimaCMD(String zInput){
		//trim it..
		String input = zInput.trim();
    	
    	//Run it..
    	JSONArray res = Command.runMultiCommand(input);
    	
    	//Get the result.. 
    	String result = MiniFormat.JSONPretty(res);
    	
		return result;
	}
	
	/**
	 * Main entry point for the Java Application
	 * 
	 * Called by fireStarter on Android
	 */
	public static void main(String[] zArgs) {
		
		//Set the main configuration folder
		File conf = new File(System.getProperty("user.home"),".minima");
		GeneralParams.CONFIGURATION_FOLDER = conf.getAbsolutePath(); 
		
		//Daemon mode
		boolean daemon = false;
		
		int arglen 	= zArgs.length;
		if(arglen > 0) {
			int counter	=	0;
			while(counter<arglen) {
				String arg 	= zArgs[counter];
				counter++;
				
				if(arg.equals("-port")) {
					GeneralParams.MINIMA_PORT = Integer.parseInt(zArgs[counter++]);
				
				}else if(arg.equals("-host")) {
					GeneralParams.MINIMA_HOST = zArgs[counter++];
				
				}else if(arg.equals("-conf")) {
					GeneralParams.CONFIGURATION_FOLDER = zArgs[counter++];
				
				}else if(arg.equals("-daemon")) {
					daemon = true;
					
				}else if(arg.equals("-private")) {
					GeneralParams.PRIVATE_NETWORK 	= true;
				
				}else if(arg.equals("-noconnect")) {
					GeneralParams.NOCONNECT = true;
					
				}else if(arg.equals("-nop2p")) {
					GeneralParams.P2P_ENABLED = false;
				
				}else if(arg.equals("-p2pnode")) {
					GeneralParams.P2P_ROOTNODE = zArgs[counter++];
					
				}else if(arg.equals("-automine")) {
					GeneralParams.AUTOMINE = true;
				
				}else if(arg.equals("-connect")) {
					GeneralParams.P2P_ENABLED  = false;
					GeneralParams.CONNECT_LIST = zArgs[counter++];
				
				}else if(arg.equals("-noautomine")) {
					GeneralParams.AUTOMINE 			= false;
				
				}else if(arg.equals("-clean")) {
					GeneralParams.CLEAN 			= true;
					
				}else if(arg.equals("-genesis")) {
					GeneralParams.CLEAN 			= true;
					GeneralParams.PRIVATE_NETWORK 	= true;
					GeneralParams.GENESIS 			= true;
					GeneralParams.AUTOMINE 			= true;
					
				}else if(arg.equals("-test")) {
					GeneralParams.TEST_PARAMS = true;
					GeneralParams.PRIVATE_NETWORK 	= true;
					TestParams.setTestParams();
				
				}else if(arg.equals("-help")) {
					
					System.out.println("Minima Help");
					System.out.println(" -host       : Specify the host IP");
					System.out.println(" -port       : Specify the Minima port");
					System.out.println(" -conf       : Specify the configuration folder");
					System.out.println(" -daemon     : Run in daemon mode with no stdin input ( services )");
					System.out.println(" -nop2p      : Disable the automatic P2P system");
					System.out.println(" -noconnect  : Stops the P2P system from connecting to other nodes until it's been connected too");
					System.out.println(" -p2pnode    : Specify the initial P2P host:port list to connect to");
					System.out.println(" -automine   : Simulate user traffic to construct the blockchain");
					System.out.println(" -noautomine : Do not simulate user traffic to construct the blockchain");
					System.out.println(" -clean      : Wipe configuration folder and all data at startup");
					System.out.println(" -genesis    : Create a genesis block, -clean and -automine");
					System.out.println(" -connect    : Disable the p2p and manually connect to this list of host:port");
					System.out.println(" -test       : Use test params");
					System.out.println(" -help       : Print this help");
					
					System.exit(1);
					
				}else {
					System.out.println("Unknown parameter : "+arg);
					System.exit(1);
				}
			}
		}
		
		MinimaLogger.log("**********************************************");
		MinimaLogger.log("*  __  __  ____  _  _  ____  __  __    __    *");
		MinimaLogger.log("* (  \\/  )(_  _)( \\( )(_  _)(  \\/  )  /__\\   *");
		MinimaLogger.log("*  )    (  _)(_  )  (  _)(_  )    (  /(__)\\  *");
		MinimaLogger.log("* (_/\\/\\_)(____)(_)\\_)(____)(_/\\/\\_)(__)(__) *");
		MinimaLogger.log("*                                            *");
		MinimaLogger.log("**********************************************");
		MinimaLogger.log("Welcome to Minima "+GlobalParams.MINIMA_VERSION+" - for assistance type help. Then press enter.");
		
		//Main handler..
		Main main = new Main();

//		Runtime.getRuntime().addShutdownHook(new Thread()
//		{
//			@Override
//			public void run()
//			{
//				MinimaLogger.log("[!] Safely Shutting Down");
//				main.shutdown();
//			}
//		});

		//Daemon mode has no stdin input
		if(daemon) {
	    	MinimaLogger.log("Daemon Started..");
			
			//Loop while running..
			while (!main.isShutdownComplete()) {
                try {Thread.sleep(1000);} catch (InterruptedException e) {}
            }
			
			//All done..
			System.exit(0);
	    }
		
		//Listen for input
		InputStreamReader is    = new InputStreamReader(System.in, MiniString.MINIMA_CHARSET);
	    BufferedReader bis      = new BufferedReader(is);
	    
	    //Loop until finished..
	    while(main.isRunning()){
	        try {
	            //Get a line of input
	            String input = bis.readLine();
	            
	            //Check valid..
	            if(input!=null && !input.equals("")) {
	            	//trim it..
	            	input = input.trim();
	            	
	            	//Run it..
	            	JSONArray res = Command.runMultiCommand(input);
	            	
	            	//Print it out 
	            	System.out.println(MiniFormat.JSONPretty(res));
	            	
	                //Is it quit..
	            	boolean quit = false;
	            	Iterator<JSONObject> it = res.iterator();
	            	while(it.hasNext()) {
	            		JSONObject json = it.next();
	            		if(json.get("command").equals("quit")) {
	            			quit = true;
	            			break;
	            		}
	            	}
	            	
	            	if(quit) {
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
	    
	    MinimaLogger.log("Bye bye..");
	}
	
}
