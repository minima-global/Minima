package org.minima.utils.bondserver;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URLEncoder;

import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManager;

import org.minima.objects.Coin;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.utils.MiniFormat;
import org.minima.utils.MinimaLogger;
import org.minima.utils.RPCClient;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.json.parser.ParseException;
import org.minima.utils.ssl.MinimaTrustManager;

public class BondServer {

	public static final String BOND_SCRIPT = "LET yourkey=PREVSTATE(100) IF SIGNEDBY(yourkey) THEN RETURN TRUE ENDIF LET maxblock=PREVSTATE(101) LET youraddress=PREVSTATE(102) LET maxcoinage=PREVSTATE(104) LET fcfinish=STATE(1) LET fcpayout=STATE(2) LET fcmilli=STATE(3) LET fccoinage=STATE(4) ASSERT fcpayout EQ youraddress ASSERT fcfinish LTE maxblock ASSERT fccoinage LTE maxcoinage LET fcaddress=0xEA8823992AB3CEBBA855D68006F0D05B0C4838FE55885375837D90F98954FA13 LET fullvalue=@AMOUNT*1.1 RETURN VERIFYOUT(@INPUT fcaddress fullvalue @TOKENID TRUE)";
	
	public static boolean mRunning 			= true;
	public static boolean mSSL 				= false;
	public static String mPassword 			= "";
	public static String mHost 				= "http://127.0.0.1:9005";
	public static SSLContext mSSLContext 	= null;
	
	public static void main(String[] zArgs) {
		
		//Are there any Args
		String sslpubkey = "";
		int arglen 	= zArgs.length;
		if(arglen > 0) {
			int counter	= 0;
			while(counter<arglen) {
				String arg 	= zArgs[counter];
				counter++;
				
				if(arg.equals("-host")) {
					mHost = zArgs[counter++];
					
				}else if(arg.equals("-password")) {
					mPassword 	= zArgs[counter++];
					
				}else if(arg.equals("-sslpubkey")) {
					sslpubkey = zArgs[counter++];
					
				}else if(arg.equals("-help")) {
					
					System.out.println("Bond Server Help");
					System.out.println(" -host       : Specify the host IP:PORT");
					System.out.println(" -password   : Specify the RPC Basic AUTH password (use with SSL)");
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
		if(mHost.startsWith("https://")) {
			mSSL = true;
			
			//Create the Trust Manager
			TrustManager[] tm = null;
			if(sslpubkey.equals("")) {
				tm = MinimaTrustManager.getTrustManagers();
			}else {
				tm = MinimaTrustManager.getTrustManagers(new MiniData(sslpubkey));				
			}
			
			//And now the SSL Context
			try {
				mSSLContext = SSLContext.getInstance("SSL");
				mSSLContext.init(null, tm, new java.security.SecureRandom());
			}catch(Exception exc) {
				MinimaLogger.log(exc);
				System.exit(1);
			}
		}
		
		//make sure host
		if(!mHost.endsWith("/")) {
			mHost = mHost+"/";
		}
		
		//Add a shutdown hook
		Runtime.getRuntime().addShutdownHook(new Thread(){
			@Override
			public void run(){
				
				//Shutdown hook called..
				if(mRunning) {
					MinimaLogger.log("[!] Shutdown Hook..");
					mRunning = false;
				}
			}
		});
		
		//Now lets go..
		System.out.println("__________ ________    _______  ________      __________________________________   _________________________ \r\n"
				+ "\\______   \\\\_____  \\   \\      \\ \\______ \\    /   _____/\\_   _____/\\______   \\   \\ /   /\\_   _____/\\______   \\\r\n"
				+ " |    |  _/ /   |   \\  /   |   \\ |    |  \\   \\_____  \\  |    __)_  |       _/\\   Y   /  |    __)_  |       _/\r\n"
				+ " |    |   \\/    |    \\/    |    \\|    `   \\  /        \\ |        \\ |    |   \\ \\     /   |        \\ |    |   \\\r\n"
				+ " |______  /\\_______  /\\____|__  /_______  / /_______  //_______  / |____|_  /  \\___/   /_______  / |____|_  /\r\n"
				+ "        \\/         \\/         \\/        \\/          \\/         \\/         \\/                   \\/         \\/ ");
		
		System.out.println("Welcome to the Minima Bond Server");
		
		//First - set up the contract and scan for it..
		try {
			JSONObject addscript = runCommand("newscript script:\""+BOND_SCRIPT+"\" trackall:true");
			if((boolean)addscript.get("status") != true) {
				MinimaLogger.log(addscript.toString());
				System.exit(1);
			}
			
		} catch (IOException | ParseException e1) {
			e1.printStackTrace();
			System.exit(1);
		}
		
		
	    //Loop until finished..
	    String result = null;
	    String input="status";
	    
	    //Keep going until finished..
	    while(mRunning) {
	    	mRunning = false;
	    	
	    	try {
	    		//The command results
	    		JSONObject jsonres = null;
	    		
	    		//What block are we on..
	    		String blockcheck 	= "status";
	    		jsonres 			= runCommand(blockcheck);
	    		JSONObject response = (JSONObject)jsonres.get("response");
	    		JSONObject chain 	= (JSONObject)response.get("chain");
	    		MiniNumber block 	= new MiniNumber((long)chain.get("block"));
	    		MinimaLogger.log("Current block : "+block.toString());
	    		
	    		//First scan for any available coins..
	    		String coincheck 	= "coins address:MxG084WU2W8JUFFKWP4WUSYKGMY1VZTR1MUY7KP9AAMAG85Q7W10NQ80R2A15PU";
	    		jsonres 			= runCommand(coincheck);
	    		JSONArray allcoins 	= (JSONArray) jsonres.get("response");
	    		
	    		int len = allcoins.size();
	    		MinimaLogger.log("Found : "+len+" Bond Request coin..");
	    		
	    		for(int i=0;i<len;i++){
	    			
	    			//Get the coin..
	    			JSONObject coinobj = (JSONObject) allcoins.get(i);
	    			
	    			//When was this coin created
	    			MiniNumber created = new MiniNumber(coinobj.getString("created"));
	    			
	    			//Check the coin age..
	    			MiniNumber coinage = block.sub(created);
	    			MinimaLogger.log("Coin: "+i+" coinid:"+coinobj.getString("coinid")+" coinage:"+coinage.toString());
	    			if(coinage.isLess(MiniNumber.TEN)) {
	    				continue;
	    			}
	    			
	    			//Spend it..
	    			String spendable 	 = "coins sendable:true tokenid:0x00";
		    		jsonres 			 = runCommand(spendable);
		    		JSONArray spendcoins = (JSONArray) jsonres.get("response");
		    		
		    		MinimaLogger.log("Found : "+spendcoins.size()+" spenable coins.. ");
		    		
	    			
	    		}
	    		
			} catch (Exception e) {
				e.printStackTrace();
			}
	    	
	    	//Pause..
	    	try {Thread.sleep(5000);} catch (InterruptedException e) {}
	    }
	}
	
	public static JSONObject runCommand(String zCommand) throws IOException, ParseException {
		
		//URLEncode..
    	String input 	= URLEncoder.encode(zCommand, MiniString.MINIMA_CHARSET);
		String full 	= mHost+input;
		
		//Now run this function..
		String result = null;
    	if(mSSL) {
    		result = RPCClient.sendGETBasicAuthSSL(full, "minima", mPassword, mSSLContext);
		}else{
    		result = RPCClient.sendGETBasicAuth(full,"minima",mPassword);
    	}
    	
    	JSONObject json = (JSONObject) new JSONParser().parse(result);
    	
    	return json;
	}
}
