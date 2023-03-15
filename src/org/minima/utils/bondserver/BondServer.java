package org.minima.utils.bondserver;

import java.io.File;
import java.io.IOException;
import java.net.URLEncoder;
import java.util.Hashtable;

import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManager;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.utils.MiniFile;
import org.minima.utils.MiniFormat;
import org.minima.utils.MinimaLogger;
import org.minima.utils.RPCClient;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.json.parser.ParseException;
import org.minima.utils.ssl.MinimaTrustManager;

public class BondServer {

	public static final String BOND_SCRIPT  = "LET yourkey=PREVSTATE(100) IF SIGNEDBY(yourkey) THEN RETURN TRUE ENDIF LET maxblock=PREVSTATE(101) LET youraddress=PREVSTATE(102) LET maxcoinage=PREVSTATE(104) LET yourrate=PREVSTATE(105) LET fcfinish=STATE(1) LET fcpayout=STATE(2) LET fcmilli=STATE(3) LET fccoinage=STATE(4) LET rate=STATE(5) ASSERT yourrate EQ rate ASSERT fcpayout EQ youraddress ASSERT fcfinish LTE maxblock ASSERT fccoinage LTE maxcoinage LET fcaddress=0xEA8823992AB3CEBBA855D68006F0D05B0C4838FE55885375837D90F98954FA13 LET fullvalue=@AMOUNT*rate RETURN VERIFYOUT(@INPUT fcaddress fullvalue @TOKENID TRUE)";
	public static final String BOND_ADDRESS = "MxG0861MPQ3ZQTM4GFTZ0UJA74Y48A4GDPYM1NTVKDTU0B34BFDV86G5A0PD21N";
	
	public static boolean mRunning 			= true;
	public static boolean mSSL 				= false;
	public static String mPassword 			= "";
	public static String mHost 				= "http://127.0.0.1:9005";
	public static SSLContext mSSLContext 	= null;
	
	public static String mFile 				= "bonds.json";
	public static boolean mTest 			= false;
	
	public static MiniNumber mMin 			= MiniNumber.ZERO;
	public static MiniNumber mMax 			= MiniNumber.ZERO;
	
	public static MiniNumber mMinCoinage 	= MiniNumber.TEN;
	
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
				
				}else if(arg.equals("-config")) {
					mFile = zArgs[counter++];
				
				}else if(arg.equals("-mincoinage")) {
					mMinCoinage = new MiniNumber(zArgs[counter++]);
					
				}else if(arg.equals("-min")) {
					mMin = new MiniNumber(zArgs[counter++]);
				
				}else if(arg.equals("-max")) {
					mMax = new MiniNumber(zArgs[counter++]);
				
				}else if(arg.equals("-test")) {
					mTest = true;
					
				}else if(arg.equals("-help")) {
					
					System.out.println("Bond Server Help");
					System.out.println(" -host       : Specify the host IP:PORT");
					System.out.println(" -password   : Specify the RPC Basic AUTH password (use with SSL)");
					System.out.println(" -sslpubkey  : The SSL public key from Minima rpc command ( if using SSL )");
					System.out.println(" -min        : Minimum amount allowed");
					System.out.println(" -max        : Maximum amount allowed");
					System.out.println(" -mincoinage : Minimum coin age before accepting - default to 10");
					System.out.println(" -config     : Location of bonds file");
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
			JSONObject addscript = runSingleCommand("newscript script:\""+BOND_SCRIPT+"\" trackall:true");
			if((boolean)addscript.get("status") != true) {
				MinimaLogger.log(addscript.toString());
				System.exit(1);
			}
			
		} catch (IOException | ParseException e1) {
			e1.printStackTrace();
			System.exit(1);
		}
		
		//Load a JSON file for the values..
		JSONArray bondjson = null;
		Hashtable<String, MiniNumber> bondrates = new Hashtable<>();
		try {
			byte[] bondvalues = MiniFile.readCompleteFile(new File(mFile));
			String bondsstr	  = new String(bondvalues);
			
			//Convert to JSON
			bondjson = (JSONArray) new JSONParser().parse(bondsstr);
			
		} catch (Exception e1) {
			e1.printStackTrace();
			System.exit(1);
		}
	
		//Convert to the hashtable
		for(Object obj : bondjson) {
			JSONObject json = (JSONObject)obj;
			
			String duration = json.getString("duration");
			String rate 	= json.getString("rate");
			
			//Add to table
			bondrates.put(rate, new MiniNumber(duration));
		}
		
		if(mTest) {
			System.out.println("TEST MODE ACTIVE - DURATION IN BLOCKS NOT DAYS");	
		}
		
		System.out.println("BOND RATES   : "+MiniFormat.JSONPretty(bondjson));
		System.out.println("TABLE        : "+bondrates.toString());
		System.out.println("MINIMUM      : "+mMin.toString());
		System.out.println("MAXIMUM      : "+mMax.toString());
		System.out.println("MIN COIN AGE : "+mMinCoinage.toString());
		
//		if(true) {
//			mRunning = false;
//			return;
//		}
		
	    //Loop until finished..
	    String result = null;
	    String input="status";
	    
	    //Keep going until finished..
	    while(mRunning) {
	    	//mRunning = false;
	    	
	    	try {
	    		//The command results
	    		JSONObject jsonres = null;
	    		
	    		//What block are we on..
	    		String blockcheck 	= "block";
	    		jsonres 			= runSingleCommand(blockcheck);
	    		JSONObject response = (JSONObject)jsonres.get("response");
	    		MiniNumber block 	= new MiniNumber(response.getString("block"));
	    		MinimaLogger.log("Current block : "+block.toString());
	    		
	    		//Get an address we can use for change
	    		String getaddress 	= "getaddress";
	    		jsonres 			= runSingleCommand(getaddress);
	    		response 			= (JSONObject)jsonres.get("response");
	    		String ouraddress	= response.getString("miniaddress");
	    		
	    		//First scan for any available coins..
	    		String coincheck 	= "coins checkmempool:true order:asc address:"+BOND_ADDRESS;
	    		jsonres 			= runSingleCommand(coincheck);
	    		JSONArray allcoins 	= (JSONArray) jsonres.get("response");
	    		//MinimaLogger.log(MiniFormat.JSONPretty(allcoins));
	    		
	    		int len = allcoins.size();
	    		MinimaLogger.log("Found : "+len+" Bond Request coin..");
	    		
	    		for(int i=0;i<len;i++){
	    			
	    			//Get the coin..
	    			JSONObject coinobj = (JSONObject) allcoins.get(i);
	    			
	    			//What is the coinid
	    			String coinid = coinobj.getString("coinid");
	    			
	    			//When was this coin created
	    			MiniNumber created = new MiniNumber(coinobj.getString("created"));
	    			MiniNumber coinamount 	= new MiniNumber(coinobj.getString("amount"));
	    			
	    			//Check the coin age..
	    			MiniNumber coinage = block.sub(created);
	    			if(coinage.isLess(mMinCoinage)) {
	    				MinimaLogger.log("[NOT OLD ENOUGH YET] Coin: "+i+" amount:"+coinamount+" coinid:"+coinid+" coinage:"+coinage.toString());
		    			continue;
	    			}
	    			
	    			//Now get the details given the rate..
		    		String rate			= getStateVar(coinobj, 105);
		    		MiniNumber ratenum 	= new MiniNumber(rate).sub(MiniNumber.ONE);
		    		
		    		//Check amount
	    			if(coinamount.isLess(mMin) || coinamount.isMore(mMax)) {
	    				MinimaLogger.log("[INVALID] Coin amount is outside allowed range "+coinamount.toString()+" coinid:"+coinid);
		    			continue;
	    			}
	    			
	    			
	    			MiniNumber reqamount  	= coinamount.mult(ratenum);
	    			MiniNumber totaltouser  = coinamount.add(reqamount);
	    			
	    			MinimaLogger.log("[VALID] coinid:"+coinid+" coinage:"+coinage.toString()+" amount:"+coinamount.toString()+" interest:"+reqamount.toString());
	    			
	    			//Spend it..
	    			String spendable 	 = "coins sendable:true tokenid:0x00 checkmempool:true";
		    		jsonres 			 = runSingleCommand(spendable);
		    		JSONArray spendcoins = (JSONArray) jsonres.get("response");
		    		
		    		MinimaLogger.log("Found : "+spendcoins.size()+" spenable coins not in mempool.. ");
		    		
		    		JSONObject ourcoin = getCoin(spendcoins, reqamount);
		    		if(ourcoin == null) {
		    			MinimaLogger.log("[WARNING] Could not find single large enough coin! "+reqamount.toString());
		    			continue;
		    		}
		    		
		    		//Construct the txn..
		    		String useraddress			= getStateVar(coinobj, 102);
		    		String ourcoinid			= ourcoin.getString("coinid");
		    		MiniNumber ourcoinamount 	= new MiniNumber(ourcoin.getString("amount"));
		    		MiniNumber ourchange 	 	= ourcoinamount.sub(reqamount);

		    		//Construct the FC params
		    		String fcaddress		= "MxG087AH0HPWAYJPQTQGYEMG03F1K2R1H43HVWYH19NB0RTW3SZWY7Q2F79810N"; 
		    		String fcusercaddress	= useraddress; 
		    		
		    		long days = 365;
		    		
		    		//Get the rate..
		    		MiniNumber duration = bondrates.get(rate);
		    		if(duration == null) {
		    			MinimaLogger.log("Invalid Rate amount! "+rate+" @ coindid:"+coinid);
		    			continue;
		    		}
		    		
		    		//How many days
		    		days = duration.getAsInt();
		    		
//		    		if(rate.equals("1.01") ){
//		    			days = 30;
//		    		}else if(rate.equals("1.035") ){
//		    			days = 90;
//		    		}else if(rate.equals("1.08") ){
//		    			days = 180;
//		    		}else if(rate.equals("1.13") ){
//		    			days = 270;
//		    		}else if(rate.equals("1.18") ){
//		    			days = 365;
//		    		}else{
//		    			MinimaLogger.log("Invalid Rate amount! "+rate+" @ coindid:"+coinid);
//		    			continue;
//		    		}
		    		
		    		//HACK
		    		//days = 0;
		    		
		    		//How many blocks in a day
		    		long dayofblocks = 1728;
		    		if(mTest) {
		    			dayofblocks = 1;
		    		}
		    		
		    		//Now calculate the max coin age - with a day extra for leeway
		    		long fccoinage = (days * dayofblocks); 
		    		
		    		//The max block
		    		long fcblock = block.getAsLong() + fccoinage;
		    		
		    		//And the time..
		    		long oneday = 1000 * 60 * 60 * 24;
		    		long fcmillitime = System.currentTimeMillis() + (days * oneday);
		    		
		    		//Check the fc vars are within the range allowed..
		    		//..
		    		
		    		//Now construct the spend txn
		    		String randid 	  = MiniData.getRandomData(32).to0xString();
		    		String txnbuilder = "txncreate id:"+randid;
		    		
		    		//Set the inputs
		    		txnbuilder 		 += ";txninput id:"+randid+" coinid:"+coinid;
		    		txnbuilder 		 += ";txninput id:"+randid+" coinid:"+ourcoinid;
		    		
		    		//Send the user his funds..
		    		txnbuilder 		 += ";txnoutput id:"+randid+" amount:"+totaltouser+" address:"+fcaddress+" tokenid:0x00";
		    		
		    		//Is there any change
		    		if(ourchange.isMore(MiniNumber.ZERO)) {
		    			txnbuilder 	 += ";txnoutput id:"+randid+" amount:"+ourchange+" address:"+ouraddress+" tokenid:0x00 storestate:false";
		    		}
		    		
		    		//Now add ther FC statevars..
		    		txnbuilder 		 += ";txnstate id:"+randid+" port:1 value:"+fcblock;
		    		txnbuilder 		 += ";txnstate id:"+randid+" port:2 value:"+fcusercaddress;
		    		txnbuilder 		 += ";txnstate id:"+randid+" port:3 value:"+fcmillitime;
		    		txnbuilder 		 += ";txnstate id:"+randid+" port:4 value:"+fccoinage;
		    		txnbuilder 		 += ";txnstate id:"+randid+" port:5 value:"+rate;
		    		
		    		//Now sign and post! the txn..
		    		txnbuilder 		 += ";txnsign id:"+randid+" publickey:auto txnpostauto:true txnpostmine:true";
		    		
		    		//And finally delete
		    		txnbuilder 		 += ";txndelete id:"+randid;
		    		
		    		//Run it..
		    		JSONArray res = runMultiCommand(txnbuilder);
		    		
		    		//Was it a success
		    		int arraylen 		= res.size();
		    		JSONObject delete 	= (JSONObject) res.get(arraylen-1);
		    		boolean status 		= (boolean) delete.get("status"); 
		    		if(status) {
		    			MinimaLogger.log("[SUCCESS SEND] : "+totaltouser.toString()+" rate:"+rate+" address:"+fcusercaddress);
		    		}else {
		    			MinimaLogger.log("[!] ERROR :");
		    			MinimaLogger.log(MiniFormat.JSONPretty(res));
		    		}
		    		
		    		//Pause..
			    	try {Thread.sleep(5000);} catch (InterruptedException e) {}
	    		}
	    		
			} catch (Exception e) {
				e.printStackTrace();
			}
	    	
	    	//Pause..
	    	try {Thread.sleep(10000);} catch (InterruptedException e) {}
	    }
	}
	
	public static JSONObject runSingleCommand(String zCommand) throws IOException, ParseException {
		
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
	
	public static JSONArray runMultiCommand(String zCommand) throws IOException, ParseException {
		
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
    	
    	JSONArray json = (JSONArray) new JSONParser().parse(result);
    	
    	return json;
	}
	
	public static JSONObject getCoin(JSONArray zSpendableCoins, MiniNumber zAmount) {
		
		//Cycle through
		for(Object obj : zSpendableCoins) {
			JSONObject coin = (JSONObject)obj;
			
			//Get the amount..
			MiniNumber amount = new MiniNumber(coin.getString("amount"));
			if(amount.isMoreEqual(zAmount)) {
				return coin;
			}
		}
		
		return null;
	}
	
	public static String getStateVar(JSONObject zCoin, long zPort) {
		JSONArray statevars = (JSONArray)zCoin.get("state");
		for(Object obj : statevars) {
			JSONObject statevar = (JSONObject)obj;
			
			//Which port..
			if((long)statevar.get("port") == zPort) {
				return statevar.getString("data");
			}
		}
		return null;
	}
}
