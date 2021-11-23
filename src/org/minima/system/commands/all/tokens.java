package org.minima.system.commands.all;

import java.util.ArrayList;

import org.minima.objects.Token;
import org.minima.objects.base.MiniNumber;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class tokens extends Command {

	public tokens() {
		super("tokens","List all tokens on the chain");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		//Get ALL the tokens in the chain..
		ArrayList<Token> alltokens = TxPoWSearcher.getAllTokens();
		
		//The return array
		JSONArray toksarr = new JSONArray();
		
		//First add Minima..
		JSONObject minima = new JSONObject();
		minima.put("name", "Minima");
		minima.put("tokenid", "0x00");
		minima.put("total", "1000000000");
		minima.put("decimals", MiniNumber.MAX_DECIMAL_PLACES);
		minima.put("scale", 1);
		toksarr.add(minima);
		
		for(Token tok : alltokens) {
			
			//Add to our list
			toksarr.add(tok.toJSON());
			
		}
		
		ret.put("response", toksarr);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new tokens();
	}

}
