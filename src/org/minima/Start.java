/**
 * 
 */
package org.minima;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Random;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.brains.BackupManager;
import org.minima.system.network.commands.CMD;
import org.minima.system.network.rpc.RPCClient;
import org.minima.utils.MiniFormat;
import org.minima.utils.MinimaLogger;
import org.minima.utils.SQLHandler;

/**
 * @author Paddy Cerri
 *
 */
public class Start {
	
	/**
	 * A list of default valid nodes to connect to at startup..
	 */
	public static String[] VALID_BOOTSTRAP_NODES = 
		{"35.204.181.120",
		 "35.204.119.15",
		 "34.91.220.49",
		 "35.204.62.177",
		 "35.204.139.141",
		 "35.204.194.45"};
	
	private static final String IPV4_PATTERN =
            "^(([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])(\\.(?!$)|$)){4}$";

    private static final Pattern pattern = Pattern.compile(IPV4_PATTERN);

    public static boolean isValidIPv4(final String email) {
        Matcher matcher = pattern.matcher(email);
        return matcher.matches();
    }
	
	/**
	 * A static link to the main server - for Android
	 */
	public static Main mMainServer = null;
	public static Main getServer() {
		return mMainServer;
	}
	
	public String mConfFolder;
	
	/**
	 * Simple constructor for iOS and Android
	 */
	public Start() {
		mMainServer = null;
	}
	
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
		/**
		 * Load the EXTRA public Minima servers for 0.98 - 0.99 has different P2P
		 */
		try {
			//Get a list of the EXTRA Minima clients..
			String extraclients = RPCClient.sendGET("http://34.118.59.216/pubextra.txt");
		
			//Now chop them up..
			ArrayList<String> pubextra = new ArrayList<>();
			StringTokenizer strtok = new StringTokenizer(extraclients,",");
			while(strtok.hasMoreElements()) {
				String ip = strtok.nextToken().trim();
				if(isValidIPv4(ip)) {
					pubextra.add(ip);
				}else {
					MinimaLogger.log("Invalid Extra Minima Peer : "+ip);
				}
			}
			
			//Now add all these nodes..
			String[] newnodes = new String[VALID_BOOTSTRAP_NODES.length+pubextra.size()];
			for(int i=0;i<VALID_BOOTSTRAP_NODES.length;i++) {
				newnodes[i] = VALID_BOOTSTRAP_NODES[i];
			}
			
			for(int i=0;i<pubextra.size();i++) {
				newnodes[VALID_BOOTSTRAP_NODES.length+i] = pubextra.get(i);
			}
			
			//Re-assign!
			VALID_BOOTSTRAP_NODES = newnodes;
			
		} catch (Exception e1) {
			MinimaLogger.log(e1);
			
			//RESET
			VALID_BOOTSTRAP_NODES = new String[]{"35.204.181.120",
												 "35.204.119.15",
												 "34.91.220.49",
												 "35.204.62.177",
												 "35.204.139.141",
												 "35.204.194.45"};
			
			MinimaLogger.log("Minima Bootstrap nodes reset : "+Arrays.toString(VALID_BOOTSTRAP_NODES));
		}
		
//		MinimaLogger.log("Minima Bootstrap Nodes : "+Arrays.toString(VALID_BOOTSTRAP_NODES));
		
		//Check command line inputs
		int arglen 				= zArgs.length;
		
		//Which port are we listening on
		int port 				= 9001;
		
		boolean connect         = true;
		
		//Pick a random host
		Random rand  = new Random();
		
		//Which Boot node
		int hostnum  = rand.nextInt(VALID_BOOTSTRAP_NODES.length);
		
		//9001, 10001, 11001 are valid ports from the BOOT nodes
		int portrand 			= rand.nextInt(3);
		ArrayList<String> connectlist = new ArrayList<>();
		String connecthost      = VALID_BOOTSTRAP_NODES[hostnum];
		int connectport         = 9001 + (1000*portrand);
		String host             = "";

		String external 		= "";
		
		boolean clean           = false;
		boolean cleanhard       = false;
		boolean privatenetwork 	= false;
		boolean daemon          = false;
		boolean automine 		= false;
		
		boolean mysql 			= false;
		String mysqlhost 		= "";
		String mysqluser 		= "";
		String mysqlpassword 	= "";
		
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
				
				}else if(arg.equals("-help")) {
					//Printout HELP!
					MinimaLogger.log("Minima "+GlobalParams.MINIMA_VERSION+" Alpha Test Net");
					MinimaLogger.log("        -port [port number]    : Specify base Minima port to listen on. RPC port will be 1 more. WebSocket Port will be 1 more..");
					MinimaLogger.log("        -host [IP]             : Specify the host IP - useful if behind firewall or on an internal network with an external IP.");
					MinimaLogger.log("        -conf [folder]         : Specify configuration folder, where data is saved.");
					MinimaLogger.log("        -private               : Run a private chain. Don't connect to MainNet. Create a genesis tx-pow. Simulate some users.");
					MinimaLogger.log("        -clean                 : Wipe user files and chain backup. Start afresh. Use with -private for clean private test-net.");
					MinimaLogger.log("        -cleanhard             : Same as -clean but remove all the MiniDAPPS.. and webroot folder");
					MinimaLogger.log("        -automine              : Simulate users mining the chain");
					MinimaLogger.log("        -noconnect             : Don't connect to MainNet. Can then connect to private chains.");
					MinimaLogger.log("        -connect [host] [port] : Don't connect to MainNet but connect to this node instead.");
					MinimaLogger.log("        -daemon                : Accepts no input from STDIN. Can run in background process.");
					MinimaLogger.log("        -mysql                 : Specify a MySQL server to use instead of built in H2 database. user:password@host");
					MinimaLogger.log("        -externalurl           : Send a POST request to this URL with Minima JSON information.");
					
					MinimaLogger.log("        -help                  : Show this help");
					MinimaLogger.log("");
					MinimaLogger.log("With zero parameters Minima will start and connect to a set of default nodes.");
					
					return;
				
				}else if(arg.equals("-private")) {
					privatenetwork  = true;
					connect 		= false;
					automine    	= true;
					
				}else if(arg.equals("-noconnect")) {
					connect = false;
				
				}else if(arg.equals("-daemon")) {
					daemon = true;
				
				}else if(arg.equals("-automine")) {
					automine = true;
				
				}else if(arg.equals("-connect")) {
					String newconn = zArgs[counter++]+":"+zArgs[counter++];
					connectlist.add(newconn);

				}else if(arg.equals("-clean")) {
					clean = true;
				
				}else if(arg.equals("-cleanhard")) {
					clean     = true;
					cleanhard = true;
					
				}else if(arg.equals("-conf")) {
					conffolder = zArgs[counter++];
				
				}else if(arg.equals("-mysql")) {
					//Get the details..
					String dets = zArgs[counter++];
					
					//Parse the details..
					int muse  = dets.indexOf(":");
					int mhost = dets.indexOf("@");
					
					if(muse==-1 || mhost==-1) {
						MinimaLogger.log("ERROR mysql format user:password@host : "+dets);
						System.exit(0);
					}
					
					//Get the details..
					mysql 			= true;
					mysqluser 		= dets.substring(0,muse);
					mysqlpassword	= dets.substring(muse+1, mhost);
					mysqlhost		= dets.substring(mhost+1);
					
//					System.out.println(mysqluser+" "+mysqlpassword+" "+mysqlhost);
										
				}else if(arg.equals("-externalurl")) {
					external = zArgs[counter++];
				
				}else if(arg.equals("-test")) {
					//Use the Test PARAMS!
					TestParams.setTestParams();
				
				}else if(arg.equals("")) {
					//Do nothing..
					
				}else {
					MinimaLogger.log("UNKNOWN arg.. : "+arg);
					System.exit(0);
				}
			}
		}

		//Are we using MYSQL
		if(mysql) {
			boolean success = SQLHandler.setMySQLDetails(mysqlhost, mysqluser, mysqlpassword);
			if(!success) {
				MinimaLogger.log("ERROR conecting to MYSQL database.. "+mysqluser+":"+mysqlpassword+"@"+mysqlhost);
				System.exit(0);
			}
		}
		
		//Configuration folder
		File conffile = new File(conffolder);
		
		//Clean up..
		if(clean) {
			BackupManager.deleteConfFolder(conffile);
		}
		
		if(cleanhard) {
			//Wipe webroot too..
			BackupManager.deleteWebRoot(conffile);
		}
		
		
		
		
		
		//Start the main Minima server
		Main rcmainserver = new Main(host, port, conffile.getAbsolutePath());
		
		//Link it.
		mMainServer = rcmainserver;
		
		//Have we added any connect hosts..
		if(connectlist.size() == 0 && connect) {
			rcmainserver.addAutoConnectHostPort(connecthost+":"+connectport);
		}else {
			for(String hostport : connectlist) {
				rcmainserver.addAutoConnectHostPort(hostport);
			}
		}
		
		//Set the connect properties
		rcmainserver.setAutoConnect(connect);
		
		//Are we private!
		if(privatenetwork) {
			//Do we need a gensis block
			boolean needgenesis = clean || BackupManager.requiresPrivateGenesis(conffile);
			
			rcmainserver.privateChain(needgenesis);
		}
		
		if(automine) {
			rcmainserver.setAutoMine();
		}
		
		if(!connect) {
			rcmainserver.setRequireNoInitialSync();
		}
		
		//Set the External URL..
		if(!external.equals("")) {
			rcmainserver.getNetworkHandler().setExternalURL(external);
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
			InputStreamReader is    = new InputStreamReader(System.in, MiniString.MINIMA_CHARSET);
		    BufferedReader bis      = new BufferedReader(is);
	
		    //Loop until finished..
		    while(rcmainserver.isRunning()){
		        try {
		            //Get a line of input
		            String input = bis.readLine();
		            
		            //Check valid..
		            if(input!=null && !input.equals("")) {
//		            	//trim it..
//		            	input = input.trim();
//		            	
//		            	//New response packet..
//			            ResponseStream response = new ResponseStream();
//			            
//		            	//Set the output stream
//			            InputMessage inmsg = new InputMessage(input, response);
//			            
//		            	//Tell main server
//		                rcmainserver.getInputHandler().PostMessage(inmsg);
//		            
//		                //Wait for the function to finish
//		                response.waitToFinish();
//		                
//		                //Get the response..
//		                String resp = response.getResponse();
		            	
		            	//trim it..
		            	input = input.trim();

//		            	System.out.println("input: "+input);
		            	
		            	//Create a Command
		            	CMD cmd = new CMD(input);
		            	
		            	//Run it..
		            	cmd.run();
		 
		            	//Get the Response..
		            	String resp = cmd.getFinalResult();
		            	
		                try {
		                	//Make the JSON pretty
			                String newresp = MiniFormat.JSONPretty(resp);
			                resp = newresp;
		                }catch(Exception exc) {
		                	//Something not right..
		                }
		                		
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
	
	public String runMinimaCMD(String zInput){
		//Create a Command
		CMD cmd = new CMD(zInput);

		//Run it.. wait for it to finish
		cmd.run();

		//Get the Response..
		String resp = cmd.getFinalResult();

		return resp;
	}
}	

