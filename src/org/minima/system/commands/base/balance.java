package org.minima.system.commands.base;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Hashtable;

import org.minima.database.MinimaDB;
import org.minima.database.txpowtree.TxPowTree;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Coin;
import org.minima.objects.Token;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.params.GeneralParams;
import org.minima.system.params.GlobalParams;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;

public class balance extends Command {

	public balance() {
		super("balance","(address:) (tokenid:) (confirmations:) (megammr:) - Show your total balance of Minima and tokens");
	}
	
	@Override
	public String getFullHelp() {
		return "\nbalance\n"
				+ "\n"
				+ "Show your total balance of Minima and tokens.\n"
				+ "\n"
				+ "address: (optional)\n"
				+ "    Show the balance for a specific 0x or Mx address.\n"
				+ "\n"
				+ "tokenid: (optional)\n"
				+ "    Show the balance for a specific tokenid. Minima is 0x00.\n"
				+ "\n"
				+ "tokendetails: (optional)\n"
				+ "    true or false, show the complete details for the tokens\n"
				+ "\n"
				+ "confirmations: (optional)\n"
				+ "    Set the number of block confirmations required before a coin is considered confirmed in your balance. Default is 3.\n"
				+ "\n"
				+ "megammr: (optional)\n"
				+ "    Search the MegaMMR for coins too.\n"
				+ "\n"
				+ "simple: (optional)\n"
				+ "    true or flase - show a much simpler view.. just the name and confirmed amount.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "balance\n"
				+ "\n"
				+ "balance simple:true\n"
				+ "\n"
				+ "balance tokenid:0xFED5.. confirmations:10\n"
				+ "\n"
				+ "balance address:0xFF..\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"address","tokenid","confirmations",
				"tokendetails","megammr","simple"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		//Is there a specified address
		String address 				= getAddressParam("address","");
		MiniNumber confirmations 	= getNumberParam("confirmations", GlobalParams.MINIMA_CONFIRM_DEPTH);
		
		//Are we in debug mode
		boolean debug  		= getBooleanParam("debug", false);
		boolean simpleview 	= getBooleanParam("simple", false);
		
		String onlytokenid = getParam("tokenid", "");
		
		//Get all the coins you own..
		TxPowTree txptree = MinimaDB.getDB().getTxPoWTree();
		if(txptree.getTip() == null) {
			throw new CommandException("No blocks yet..");
		}
		
		//The final Balances of ALl tokens..
		JSONArray balance = new JSONArray();
		
		//Was there an Exception
		Exception texc = null;
		
		//Keep track of totals..
		ArrayList<String> alltokens 				= new ArrayList<>();
		Hashtable<String, Token> tokens 			= new Hashtable<>();
		Hashtable<String, MiniNumber> confirmed 	= new Hashtable<>();
		Hashtable<String, MiniNumber> unconfirmed 	= new Hashtable<>();
		Hashtable<String, MiniNumber> sendable 		= new Hashtable<>();
		Hashtable<String, MiniNumber> totalcoins 	= new Hashtable<>();
		
		//Get the wallet.. to find the sendable coins..
		Wallet walletdb = MinimaDB.getDB().getWallet();
		
		//Do we check the MegaMMR
		boolean checkmegammr = getBooleanParam("megammr", false);
		if(checkmegammr) {
			checkmegammr = GeneralParams.IS_MEGAMMR;
		}
		
		//Get the coins..
		ArrayList<Coin> coins = null;
		if(address.equals("")) {
			coins = TxPoWSearcher.getAllRelevantUnspentCoins(txptree.getTip());
		}else {
			
			//Special search
			coins = TxPoWSearcher.searchCoins(	txptree.getTip(), false, 
					false, MiniData.ZERO_TXPOWID,
					false, MiniNumber.ZERO,
					true, new MiniData(address), 
					false, MiniData.ZERO_TXPOWID, 
					false, "", true,
					false, Integer.MAX_VALUE,checkmegammr);
		}
			
		//What is the top block
		MiniNumber topblock = txptree.getTip().getBlockNumber();
		
		//Always show a Minima Balance
		alltokens.add(Token.TOKENID_MINIMA.to0xString());
		totalcoins.put(Token.TOKENID_MINIMA.to0xString(), MiniNumber.ZERO);
		
		if(debug) {
			MinimaLogger.log("List of found relevant coins");
		}
		//Add them to out balance..
		for(Coin coin : coins) {
			
			//Are we checking this coin..
			if(!address.equals("")) {
				if(!coin.getAddress().to0xString().equals(address)) {
					continue;
				}
			}
			
			if(!onlytokenid.equals("")) {
				if(!coin.getTokenID().to0xString().equals(onlytokenid)) {
					continue;
				}
			}
			
			if(debug) {
				MinimaLogger.log("Coin : "+coin.toJSON().toString());
			}
			
			//The Value..
			MiniNumber amount = coin.getAmount();
			
			//Which Token..
			String tokenid = coin.getTokenID().to0xString();
			if(!alltokens.contains(tokenid)) {
				//Add to out token list
				alltokens.add(tokenid);
				tokens.put(tokenid, coin.getToken());
			}
			
			//How deep is this coin
			MiniNumber depth = topblock.sub(coin.getBlockCreated());
			
			//Which table are we updating
			boolean isconfirmed = true;
			Hashtable<String, MiniNumber> current = confirmed;
			if(depth.isLess(confirmations)) {
				current = unconfirmed;
				isconfirmed = false;
			}
			
			//Have we already added..
			MiniNumber total 	= current.get(tokenid); 
			if(total == null) {
				current.put(tokenid, amount);
			}else {
				current.put(tokenid, total.add(amount));
			}
			
			//Total coins.
			MiniNumber totcoin 	= totalcoins.get(tokenid); 
			if(totcoin == null) {
				totalcoins.put(tokenid, MiniNumber.ONE);
			}else {
				totalcoins.put(tokenid, totcoin.increment());
			}
			
			//Are we adding to the sendable pile..
			if(debug) {
				boolean simple = walletdb.isAddressSimple(coin.getAddress().to0xString());
				if(!simple) {
					MinimaLogger.log("NON-SENDABLE : "+coin.toJSON().toString());
				}else {
					MinimaLogger.log("SENDABLE : "+coin.toJSON().toString());
				}
			}
			
			if(tokenid.equals("0x00")) {
				if(isconfirmed && walletdb.isAddressSimple(coin.getAddress().to0xString())) {
					total = sendable.get(tokenid); 
					if(total == null) {
						sendable.put(tokenid, amount);
					}else {
						sendable.put(tokenid, total.add(amount));
					}
				}
			}else {
				
				//Check tokenscript..
				String script = coin.getToken().getTokenScript().toString();
				if(script.equals("RETURN TRUE")) {
					if(isconfirmed && walletdb.isAddressSimple(coin.getAddress().to0xString())) {
						total = sendable.get(tokenid); 
						if(total == null) {
							sendable.put(tokenid, amount);
						}else {
							sendable.put(tokenid, total.add(amount));
						}
					}
				}
			}
		}
		
		//Do we print ALL the token details..
		boolean tokendetails = getBooleanParam("tokendetails", false);
		
		//Lets print out..
		for(String token : alltokens) {
			
			MiniNumber unconf 	= unconfirmed.get(token);
			MiniNumber conf 	= confirmed.get(token);
			MiniNumber send 	= sendable.get(token);
			MiniNumber totcoins = totalcoins.get(token);
			
			if(unconf == null) {
				unconf = MiniNumber.ZERO;
			}
			
			if(conf == null) {
				conf = MiniNumber.ZERO;
			}
			
			if(send == null) {
				send = MiniNumber.ZERO;
			}
			
			JSONObject tokbal = new JSONObject();
			
			if(token.equals("0x00")) {
				if((onlytokenid.equals("") || onlytokenid.equals("0x00"))) {
					//It's Minima
					tokbal.put("token", "Minima");
					tokbal.put("tokenid", token);
					tokbal.put("confirmed", conf.toString());
					tokbal.put("unconfirmed", unconf.toString());
					tokbal.put("sendable", send.toString());
					tokbal.put("coins", totcoins.toString());
					tokbal.put("total", "1000000000");
					
					//And add to the total..
					balance.add(tokbal);
				}
			}else {
				//Get the token
				Token tok = tokens.get(token);
				
				if(tok.getName().toString().trim().startsWith("{")) {
					
					//It's a JSON
					tokbal.put("token", tok.getName());
				}else {
					//It's a String
					tokbal.put("token", tok.getName().toString());
					
				}
				
				tokbal.put("token", tok.getName());
				tokbal.put("tokenid", token);
				tokbal.put("confirmed", tok.getScaledTokenAmount(conf).toString());
				tokbal.put("unconfirmed", tok.getScaledTokenAmount(unconf).toString());
				tokbal.put("sendable", tok.getScaledTokenAmount(send).toString());
				tokbal.put("coins", totcoins.toString());
				tokbal.put("total", tok.getTotalTokens().toString());
				
				if(tokendetails) {
					JSONObject tdetails = new JSONObject();
					tdetails.put("decimals", tok.getDecimalPlaces());
					tdetails.put("script", tok.getTokenScript().toString());
					tdetails.put("totalamount", tok.getAmount().toString());
					tdetails.put("scale", tok.getScale().toString());
					tdetails.put("created", tok.getCreated().toString());
					
					tokbal.put("details", tdetails);
				}
				
				//And add to the total..
				balance.add(tokbal);
			}
		}
		
		//Make it simple..
		if(simpleview) {
			
			JSONArray simplebalance = new JSONArray();
			for(Object bal : balance) {
				JSONObject oldbal = (JSONObject)bal;
				JSONObject newbal = new JSONObject();
				
				//What is the tokenid
				if(oldbal.getString("tokenid").equals("0x00")) {
					newbal.put("name","Minima");
				}else {
					MiniString tok 		= (MiniString) oldbal.get("token");
					JSONObject tokobj 	= (JSONObject) new JSONParser().parse(tok.toString());
					newbal.put("name",tokobj.get("name"));
				}
				
				//Only add the Confirmed amount
				newbal.put("amount",oldbal.getString("confirmed"));
				
				//Add to the Simple balance..
				simplebalance.add(newbal);
			}
			
			//Add balance..
			ret.put("response", simplebalance);
			
		}else {
			//Add balance..
			ret.put("response", balance);
		}
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new balance();
	}

}
