package org.minima.system.commands.all;

import java.util.ArrayList;
import java.util.Hashtable;

import org.minima.database.MinimaDB;
import org.minima.database.txpowtree.TxPowTree;
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
		Hashtable<String, MiniNumber> confirmed 	= new Hashtable<>();
		Hashtable<String, MiniNumber> unconfirmed 	= new Hashtable<>();
		
		//No change allowed whilst we are doing this..
		MinimaDB.getDB().readLock(true);
		
		ArrayList<Coin> coins = null;
		try {
			//Get all the Unspent Coins..
			coins = TxPoWSearcher.getRelevantUnspentCoins(txptree.getTip());
		
		}catch(Exception exc) {
			texc = exc;
		}
		
		//No change allowed whilst we are doing this..
		MinimaDB.getDB().readLock(false);
		
		//Check for exception..
		if(texc != null) {
			throw texc;
		}
			
		//What is the top block
		MiniNumber topblock = txptree.getTip().getBlockNUmber();
		
		//Always show a Minima Balance
		alltokens.add(Token.TOKENID_MINIMA.to0xString());
		
		//Add them to out balance..
		for(Coin coin : coins) {
			
			//The Value..
			MiniNumber amount = coin.getAmount();
			
			//Which Token..
			String tokenid = coin.getTokenID().to0xString();
			if(!alltokens.contains(tokenid)) {
				alltokens.add(tokenid);
			}
			
			//How deep is this coin
			MiniNumber depth = topblock.sub(coin.getBlockCreated());
			
			//Which table are we updating
			Hashtable<String, MiniNumber> current = confirmed;
			if(depth.isLess(GlobalParams.MINIMA_CONFIRM_DEPTH)) {
				current = unconfirmed;
			}
			
			//Have we already added..
			MiniNumber total = current.get(tokenid); 
			if(total == null) {
				current.put(tokenid, amount);
			}else {
				current.put(tokenid, total.add(amount));
			}
		}
		
		//Lets print out..
		for(String token : alltokens) {
			
			MiniNumber unconf 	= unconfirmed.get(token);
			MiniNumber conf 	= confirmed.get(token);
			
			if(unconf == null) {
				unconf = MiniNumber.ZERO;
			}
			
			if(conf == null) {
				conf = MiniNumber.ZERO;
			}
			
			JSONObject tokbal = new JSONObject();
			tokbal.put("tokenid", token);
			tokbal.put("confirmed", conf.toString());
			tokbal.put("unconfirmed", unconf.toString());
			
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
