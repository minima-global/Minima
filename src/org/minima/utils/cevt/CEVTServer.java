package org.minima.utils.cevt;

import java.io.IOException;
import java.net.URLEncoder;

import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManager;

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

public class CEVTServer {

	//Use MAXCREATE to get these
	public static String CEVT_PUBKEY 		= "0x30819F300D06092A864886F70D010101050003818D0030818902818100AECB70FAD27C2F88AAA2DCB908D4EE89E0C6D0D92C6EAEE9CD1332708CAA618BF5C0ED54180BE1D1957B027DFEEC142561269A4B1EACFC985FFAB8BA653EAFC3FACFCC090E7FB62E0435CD13C81CC2B2383E41E3FD999A5EF91EFE7F7E357C9D72C16D773667098D145F0F08F7EF2F6D29B1328C83BFABCF60AE2F88B2FDB2030203010001";
	public static String CEVT_PRIVATE_KEY 	= "0x30820275020100300D06092A864886F70D01010105000482025F3082025B02010002818100AECB70FAD27C2F88AAA2DCB908D4EE89E0C6D0D92C6EAEE9CD1332708CAA618BF5C0ED54180BE1D1957B027DFEEC142561269A4B1EACFC985FFAB8BA653EAFC3FACFCC090E7FB62E0435CD13C81CC2B2383E41E3FD999A5EF91EFE7F7E357C9D72C16D773667098D145F0F08F7EF2F6D29B1328C83BFABCF60AE2F88B2FDB20302030100010281807E40235287457B6FD30FDF2D26DBE58F60F339562369AE9CE0AAC4FBD61E1A66E5127C5909254BEDFD71E3D2ED95C6D758DC7105611AC137ADDB8221DF8EEF9401C7B07B3C01DC3481094069F6FCEB1F40C55EF82EA59D25890A533423714582E71819806FE6ECADB2772C0DACDBD164179ACF52BDF021189BF2BB57A89EF941024100F2FA639C597E85A0F72FCE58A48B16F8F9CB9F3AFD9E16A2877EA225AD579CE74C4D81C17B1C0C1769D0057FFFCBA487556D5879E563E2DEC2C4E01B71859D9F024100B82996F64F2C955E1F82811D65BA3DFB0779DCB1258C6EB134682DFA98B8F6A9AA38E76166741134F23908BAE471F2D23982F031C8C94042E141D5AA6947C91D02402A85711B327A36CE7593B32BC617E53C4B12BC59E97A5A01B56E8ED7C31F1B014B54210A080E5F7B55742712406495F56D18C6AC065DD59D203EADED7F8F5F870240140B4AC802340B09D48A22BD6D5E08F2F28C5BEDD7F561F2DD8E5D3AC236977A3DF1B5954F4E60B50F8C0F9C2FD30B27C064B4F9E62DA955DE288935E401912D0240195E91C4A960D8CC7CBFD4C44A7813A0D1D99F3CE58BABC7C5E760881653D4F5224AE1464947119A90A512F9E8D60D41383E41D57233BA5BACA9E50C1FF9E99E";
	
	//Must be Specified
	public static String CEVT_ADDRESS			= "";
	public static String CEVT_CHANGE_ADDRESS	= "";
	public static String KJ_TOKENID				= "";
	public static String KJ_RECEIPT_TOKENID		= "";
	
	public static MiniNumber mMinCoinage 	= MiniNumber.ONE;
	
	public static boolean mRunning 			= true;
	public static boolean mSSL 				= false;
	public static String mPassword 			= "";
	public static String mHost 				= "http://127.0.0.1:10005";
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
				
				}else if(arg.equals("-mainaddress")) {
					CEVT_ADDRESS = zArgs[counter++];
				
				}else if(arg.equals("-changeaddress")) {
					CEVT_CHANGE_ADDRESS = zArgs[counter++];
				
				}else if(arg.equals("-kjtoken")) {
					KJ_TOKENID = zArgs[counter++];
				
				}else if(arg.equals("-kjreceipt")) {
					KJ_RECEIPT_TOKENID = zArgs[counter++];
				
				}else if(arg.equals("-mincoinage")) {
					mMinCoinage = new MiniNumber(zArgs[counter++]);
					
				}else if(arg.equals("-help")) {
					
					System.out.println("CEVT Server Help");
					System.out.println(" -host           : Specify the host IP:PORT");
					System.out.println(" -password       : Specify the RPC Basic AUTH password (use with SSL)");
					System.out.println(" -sslpubkey      : The SSL public key from Minima rpc command ( if using SSL )");
					
					System.out.println(" -mainaddress    : The MAIN address to check for KJ sends");
					System.out.println(" -changeaddress  : The change address to use when sending funds");
					
					System.out.println(" -kjtoken        : The Killer Joules tokenid");
					System.out.println(" -kjreceipt      : The NFT tokenid when sending back the ACT");
					
					System.out.println(" -mincoinage     : Minimum coin age before accepting - default to 1");
					
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
		System.out.println("\r\n"
				+ "_________ _______________   _______________   __________________________________   _________________________ \r\n"
				+ "\\_   ___ \\\\_   _____/\\   \\ /   /\\__    ___/  /   _____/\\_   _____/\\______   \\   \\ /   /\\_   _____/\\______   \\\r\n"
				+ "/    \\  \\/ |    __)_  \\   Y   /   |    |     \\_____  \\  |    __)_  |       _/\\   Y   /  |    __)_  |       _/\r\n"
				+ "\\     \\____|        \\  \\     /    |    |     /        \\ |        \\ |    |   \\ \\     /   |        \\ |    |   \\\r\n"
				+ " \\______  /_______  /   \\___/     |____|    /_______  //_______  / |____|_  /  \\___/   /_______  / |____|_  /\r\n"
				+ "        \\/        \\/                                \\/         \\/         \\/                   \\/         \\/ \r\n"
				+ "");
		
		System.out.println("Welcome to the CEVT Server");
		
		System.out.println("Main Address    : "+CEVT_ADDRESS);
		System.out.println("Change Address  : "+CEVT_CHANGE_ADDRESS);
		System.out.println("KJ TokenID      : "+KJ_TOKENID);
		System.out.println("KJ RECEIPT NFT  : "+KJ_RECEIPT_TOKENID);
		System.out.println("Min coin age    : "+mMinCoinage.toString());
		
		//Check params
		if(KJ_TOKENID.equals("")) {
			System.out.println("MUST specify the KJ TokenID!..");
			System.exit(1);
		
		}else if(KJ_RECEIPT_TOKENID.equals("")) {
			System.out.println("MUST specify the KJ Receipt TokenID!..");
			System.exit(1);
		
		}else if(CEVT_ADDRESS.equals("")) {
			System.out.println("MUST specify the CEVT Main address!..");
			System.exit(1);
		
		}else if(CEVT_CHANGE_ADDRESS.equals("")) {
			System.out.println("MUST specify the CEVT Change address!..");
			System.exit(1);
		}
		
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
	    		String coincheck 	= "coins relevant:true tokenid:"+KJ_TOKENID+" checkmempool:true order:asc address:"+CEVT_ADDRESS;
	    		jsonres 			= runSingleCommand(coincheck);
	    		JSONArray allcoins 	= (JSONArray) jsonres.get("response");
	    		
	    		int len = allcoins.size();
	    		MinimaLogger.log("Found : "+len+" CEVT Request coin..");
	    		
	    		for(int i=0;i<len;i++){
	    			
	    			//Get the coin..
	    			JSONObject coinobj = (JSONObject) allcoins.get(i);
	    			
	    			//What is the coinid
	    			String coinid = coinobj.getString("coinid");
	    			
	    			//When was this coin created
	    			MiniNumber created 		= new MiniNumber(coinobj.getString("created"));
	    			MiniNumber tokenamount 	= new MiniNumber(coinobj.getString("tokenamount"));
	    			
	    			//Check the coin age..
	    			MiniNumber coinage = block.sub(created);
	    			if(coinage.isLess(mMinCoinage)) {
	    				MinimaLogger.log("[NOT OLD ENOUGH YET] Coin: "+i+" amount:"+tokenamount+" coinid:"+coinid+" coinage:"+coinage.toString());
		    			continue;
	    			}
	    			
	    			//Now get the details
		    		String useraddress 	= getStateVar(coinobj, 0);
		    		String dterminalid 	= getStateVar(coinobj, 1);
		    		String userpubkey  	= getStateVar(coinobj, 2);
		    		
	    			MinimaLogger.log("[VALID] coinid:"+coinid+" coinage:"+coinage.toString()+" amount:"+tokenamount.toString()+" terminal:"+dterminalid);
	    			
	    			//OK - Send him back an NFT with required data
	    			String datapackage = "#:"+dterminalid+":"+userpubkey+":"+tokenamount+":"+coinid;
	    			
	    			//Hash that
	    			jsonres 	= runSingleCommand("hash data:"+datapackage);
	    			response 	= (JSONObject)jsonres.get("response");
	    			String hash = response.getString("hash");
	    			
	    			MinimaLogger.log(response.toString());
	    			
	    			//Sign that hash..
	    			jsonres 			= runSingleCommand("maxsign data:"+hash+" privatekey:"+CEVT_PRIVATE_KEY);
	    			response 			= (JSONObject)jsonres.get("response");
	    			String signature 	= response.getString("signature");
	    			
	    			//Now construct and send back..
	    			String spendable 	 = "coins coinage:5 sendable:true tokenid:"+KJ_RECEIPT_TOKENID+" checkmempool:true";
		    		jsonres 			 = runSingleCommand(spendable);
		    		JSONArray spendcoins = (JSONArray) jsonres.get("response");
		    		
		    		MinimaLogger.log("Found : "+spendcoins.size()+" spenable coins not in mempool.. ");
		    		
		    		JSONObject ourcoin = getCoin(spendcoins, MiniNumber.ONE);
		    		if(ourcoin == null) {
		    			MinimaLogger.log("[WARNING] Could not find single spendable large enough coin! (1)");
		    			continue;
		    		}
		    		
		    		//How much change
		    		String ourcoinid			= ourcoin.getString("coinid");
		    		MiniNumber ourcoinamount 	= new MiniNumber(ourcoin.getString("tokenamount"));
		    		MiniNumber ourchange 	 	= ourcoinamount.sub(MiniNumber.ONE);

		    		//Now construct the spend txn
		    		String randid 	  = MiniData.getRandomData(32).to0xString();
		    		String txnbuilder = "txncreate id:"+randid;
		    		
		    		//Set the inputs - the 1 we send and the kj coins we send back to ourselves..
		    		txnbuilder 		 += ";txninput id:"+randid+" coinid:"+ourcoinid;
		    		txnbuilder 		 += ";txninput id:"+randid+" coinid:"+coinid;
		    		
		    		//Send the user his funds..
		    		txnbuilder 		 += ";txnoutput id:"+randid+" amount:1 address:"+useraddress+" tokenid:"+KJ_RECEIPT_TOKENID;
		    		
		    		//Send us back the coins - to a different address
		    		txnbuilder 		 += ";txnoutput id:"+randid+" amount:"+tokenamount+" address:"+CEVT_CHANGE_ADDRESS+" tokenid:"+KJ_TOKENID;
		    		
		    		//Is there any change
		    		if(ourchange.isMore(MiniNumber.ZERO)) {
		    			txnbuilder 	 += ";txnoutput id:"+randid+" amount:"+ourchange+" address:"+CEVT_CHANGE_ADDRESS+" tokenid:"+KJ_RECEIPT_TOKENID+" storestate:false";
		    		}
		    		
		    		//Now add ther FC statevars..
		    		txnbuilder 		 += ";txnstate id:"+randid+" port:0 value:"+dterminalid;
		    		txnbuilder 		 += ";txnstate id:"+randid+" port:1 value:"+userpubkey;
		    		txnbuilder 		 += ";txnstate id:"+randid+" port:2 value:"+tokenamount;
		    		txnbuilder 		 += ";txnstate id:"+randid+" port:3 value:"+coinid;
		    		txnbuilder 		 += ";txnstate id:"+randid+" port:4 value:"+hash;
		    		txnbuilder 		 += ";txnstate id:"+randid+" port:5 value:"+signature;
		    		
		    		//Now sign and post! the txn..
		    		txnbuilder 		 += ";txnsign id:"+randid+" publickey:auto txnpostauto:true txnpostmine:true";
		    		
		    		//And finally delete
		    		txnbuilder 		 += ";txndelete id:"+randid;
		    		
		    		MinimaLogger.log(txnbuilder);
		    		
		    		//Run it..
		    		JSONArray res = runMultiCommand(txnbuilder);
		    		
		    		//Was it a success
		    		int arraylen 		= res.size();
		    		JSONObject delete 	= (JSONObject) res.get(arraylen-1);
		    		boolean status 		= (boolean) delete.get("status"); 
		    		if(status) {
		    			MinimaLogger.log("[SUCCESS SEND] : "+tokenamount+" address:"+useraddress);
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
			MiniNumber amount = new MiniNumber(coin.getString("tokenamount"));
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
