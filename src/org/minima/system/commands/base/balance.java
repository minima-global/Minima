package org.minima.system.commands.base;

import java.util.ArrayList;
import java.util.Hashtable;

import org.minima.database.MinimaDB;
import org.minima.database.txpowtree.TxPowTree;
import org.minima.database.wallet.KeyRow;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Coin;
import org.minima.objects.Token;
import org.minima.objects.base.MiniNumber;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.commands.Command;
import org.minima.system.params.GlobalParams;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class balance extends Command {

	public balance() {
		super("balance","Show your total balance of Minima and tokens");
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		//Get all the coins you own..
		TxPowTree txptree = MinimaDB.getDB().getTxPoWTree();
		
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
		
		//Get the wallet.. to find the sendable coins..
		Wallet wdb = MinimaDB.getDB().getWallet();
		ArrayList<KeyRow> keys 					= wdb.getAllRelevant(false);
		ArrayList<String> sendableaddresses 	= new ArrayList<>();
		for(KeyRow key : keys) {
			if(!key.getPublicKey().equals("")) {
				sendableaddresses.add(key.getAddress());
			}
		}
		
		//Get the coins..
		ArrayList<Coin> coins = TxPoWSearcher.getAllRelevantUnspentCoins(txptree.getTip());
			
		//What is the top block
		MiniNumber topblock = txptree.getTip().getBlockNumber();
		
		//Always show a Minima Balance
		alltokens.add(Token.TOKENID_MINIMA.to0xString());
		
		//Add them to out balance..
		for(Coin coin : coins) {
			
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
			if(depth.isLess(GlobalParams.MINIMA_CONFIRM_DEPTH)) {
				current = unconfirmed;
				isconfirmed = false;
			}
			
			//Have we already added..
			MiniNumber total = current.get(tokenid); 
			if(total == null) {
				current.put(tokenid, amount);
			}else {
				current.put(tokenid, total.add(amount));
			}
			
			//Are we adding to the sendable pile..
			if(isconfirmed && sendableaddresses.contains(coin.getAddress().to0xString())) {
				total = sendable.get(tokenid); 
				if(total == null) {
					sendable.put(tokenid, amount);
				}else {
					sendable.put(tokenid, total.add(amount));
				}
			}
		}
		
		//Lets print out..
		for(String token : alltokens) {
			
			MiniNumber unconf 	= unconfirmed.get(token);
			MiniNumber conf 	= confirmed.get(token);
			MiniNumber send 	= sendable.get(token);
			
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
				//It's Minima
				tokbal.put("token", "Minima");
				tokbal.put("tokenid", token);
				tokbal.put("confirmed", conf.toString());
				tokbal.put("unconfirmed", unconf.toString());
				tokbal.put("sendable", send.toString());
				tokbal.put("total", "1000000000");
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
				tokbal.put("total", tok.getTotalTokens());
			}
			
			//And add to the total..
			balance.add(tokbal);
		}
		
		//Add balance..
		ret.put("response", balance);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new balance();
	}

}
