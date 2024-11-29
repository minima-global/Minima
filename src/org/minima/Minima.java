package org.minima;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Iterator;

import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.commands.CommandRunner;
import org.minima.system.params.GeneralParams;
import org.minima.system.params.GlobalParams;
import org.minima.system.params.ParamConfigurer;
import org.minima.utils.MiniFormat;
import org.minima.utils.MinimaLogger;
import org.minima.utils.MinimaUncaughtException;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class Minima {

	private static InputStreamReader mInputStream = null;
	private static BufferedReader mBufferedStream = null;
    
	private static boolean mIsRunning = true;
	
	public Minima() {}
	
	/**
	 * Call main() with a set of variables
	 */
	public void mainStarter(final String[] zArgs) {
		
		//Create a separate thread
		Runnable mainrunner = new Runnable() {
			@Override
			public void run() {
				//And call it..
				main( zArgs );
			}
		};
		
		//Run it..
		Thread mainthread=new Thread(mainrunner);
		mainthread.start();
	}
	
	/**
	 * Get ther Main class
	 */
	public static Main getMain() {
		return Main.getInstance();
	}
	
	/**
	 * Run a command on Minima and return the result
	 */
	
	public String runMinimaCMD(String zInput){
		return runMinimaCMD(zInput, true);
	}
	
	public String runMinimaCMD(String zInput, boolean zPrettyJSON){
		//trim it..
		String input = zInput.trim();
    	
    	//Run it..
    	JSONArray res = CommandRunner.getRunner().runMultiCommand(input);
    	
    	//Get the result.. 
    	String result = null;
    	if(zPrettyJSON) {
    		result = MiniFormat.JSONPretty(res);
    	}else {
    		if(res.size() == 1) {
    			result = res.get(0).toString();
    		}else {
    			result = res.toJSONString();
    		}
    	}
    	
		return result;
	}
	
	/**
	 * Main entry point for the Java Application
	 * 
	 * Called by fireStarter on Android
	 */
	public static void main(String[] zArgs) {
		
		//we are running
		mIsRunning = true;
		
		//Set the main data folder
		File dataFolder 	= new File(System.getProperty("user.home"),".minima");
		
		//Depends on the VERSION
		File minimafolder 	= new File(dataFolder,GlobalParams.MINIMA_BASE_VERSION);
		
		//Reset ALL the GeneralParams - ANDROID KEEPS THEM after a shutdown
		GeneralParams.resetDefaults();
		
		//Set this globally
		GeneralParams.DATA_FOLDER 	= minimafolder.getAbsolutePath();
		
		//Run Params configure
		ParamConfigurer configurer = null;
		try {
			configurer = new ParamConfigurer()
					.usingConfFile(zArgs)
					.usingEnvVariables(System.getenv())
					.usingProgramArgs(zArgs)
					.configure();
		} catch (ParamConfigurer.UnknownArgumentException ex) {
			System.out.println(ex);
			System.exit(1);
		}

		//Make sure the main DATA folder exists..
		File maindata = new File(GeneralParams.DATA_FOLDER);
		maindata.mkdirs();
		
		boolean daemon 			= configurer.isDaemon();
		boolean shutdownhook 	= configurer.isShutDownHook();

		//Set the Ports.. If Minima port has changed
		GeneralParams.MDSFILE_PORT 		= GeneralParams.MINIMA_PORT+2;
		GeneralParams.MDSCOMMAND_PORT 	= GeneralParams.MINIMA_PORT+3;
		GeneralParams.RPC_PORT 			= GeneralParams.MINIMA_PORT+4;
		
		//Now lets go..
		MinimaLogger.log("**********************************************");
		MinimaLogger.log("*  __  __  ____  _  _  ____  __  __    __    *");
		MinimaLogger.log("* (  \\/  )(_  _)( \\( )(_  _)(  \\/  )  /__\\   *");
		MinimaLogger.log("*  )    (  _)(_  )  (  _)(_  )    (  /(__)\\  *");
		MinimaLogger.log("* (_/\\/\\_)(____)(_)\\_)(____)(_/\\/\\_)(__)(__) *");
		MinimaLogger.log("*                                            *");
		MinimaLogger.log("**********************************************");
		MinimaLogger.log("Welcome to Minima v"+GlobalParams.MINIMA_VERSION+" - for assistance type help. Then press enter.");
		
		//Load the required MySQL classes
		try {
			Class.forName("com.mysql.cj.jdbc.Driver");
		} catch (ClassNotFoundException e1) {
			//e1.printStackTrace();
		}
		
		//Catch ALL Uncaught Exceptions..
		Thread.setDefaultUncaughtExceptionHandler(new MinimaUncaughtException());
		
		//TEST ERRORS - not used for now
		/*List<byte[]> list = new ArrayList<>();
		int index = 1;
		while (index < 100000) {
				// 1MB each loop, 1 x 1024 x 1024 = 1048576
				byte[] b = new byte[10485760];
				list.add(b);
				Runtime rt = Runtime.getRuntime();
				System.out.printf("[%d] free memory: %s%n", index++, rt.freeMemory());
		}
		
		byte[] pp = null;
		if(true) {
			int len = pp.length;
			MinimaLogger.log("PP allocated : "+MiniFormat.formatSize(len));
		}*/
		
		//Main handler..
		Main main = new Main();
		
		//A shutdown hook..
		if(shutdownhook) {
			Runtime.getRuntime().addShutdownHook(new Thread(){
				@Override
				public void run(){
					//Are we already shutting down..
					if(main.isShuttingDown()) {
						return;
					}
					
					//Shutdown hook called..
					MinimaLogger.log("[!] Shutdown Hook..");
					
					//Shut down the whole system
					main.shutdown();
				}
			});
		}

		//Daemon mode has no stdin input
		if(daemon) {
	    	MinimaLogger.log("Daemon mode started..");
			
			//Loop while running..
			while (!main.isShutdownComplete()) {
                try {Thread.sleep(250);} catch (InterruptedException e) {}
            }
			
			//All done.. The shutdown hook will exit the system..
			return;
	    }
		
		//Listen for input
		mInputStream    = new InputStreamReader(System.in, MiniString.MINIMA_CHARSET);
	    mBufferedStream = new BufferedReader(mInputStream);
	    
	    //Loop until finished..
	    while(mIsRunning && main.isRunning()){
	        try {
	            //Get a line of input
	            String input = mBufferedStream.readLine();
	            
	            //Check valid..
	            if(input!=null && !input.equals("")) {
	            	//trim it..
	            	input = input.trim();
	            	
	            	//Run it..
	            	JSONArray res = CommandRunner.getRunner().runMultiCommand(input);
	            	
	            	//Print it out
	            	if(!input.equals("quit")) {
	            		System.out.println(MiniFormat.JSONPretty(res));
	            	}
	            	
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
	        mBufferedStream.close();
	        mInputStream.close();
	    } catch (IOException ex) {
	    	MinimaLogger.log(""+ex);
	    }
	    
	    MinimaLogger.log("Minima CLI input stopped.. ",false);
	}
}
