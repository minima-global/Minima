package org.minima.system.commands.search;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.objects.Token;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class tokens extends Command {

	public tokens() {
		super("tokens","(tokenid:) (action:import|export) (data:) - List, import or export tokens on the chain");
	}
	
	@Override
	public String getFullHelp() {
		return "\ntokens\n"
				+ "\n"
				+ "List all tokens in the unpruned chain.\n"
				+ "\n"
				+ "Optionally import or export tokens to share token data.\n"
				+ "\n"
				+ "tokenid: (optional)\n"
				+ "    The tokenid of the token to search for or export.\n"
				+ "\n"
				+ "action: (optional)\n"
				+ "    import : List your existing public keys.\n"
				+ "    export : Create a new key.\n"
				+ "\n"
				+ "data: (optional)\n"
				+ "    The data of the token to import, generated from the export.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "tokens\n"
				+ "\n"
				+ "tokens tokenid:0xFED5..\n"
				+ "\n"
				+ "tokens action:export tokenid:0xFED5..\n"
				+ "\n"
				+ "tokens action:import data:0x000..\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"tokenid","action","data"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		String tokenid = getParam("tokenid","");
		String action  = getParam("action", "");
		
		if(action.equals("export")) {
			
			//Export a token..
			Token tok = TxPoWSearcher.getToken(new MiniData(tokenid));
			if(tok == null) {
				throw new CommandException("Token not found : "+tokenid);
			}
			
			//Ok  - now convert to MiniData..
			MiniData tokdata = MiniData.getMiniDataVersion(tok);
			
			JSONObject resp = new JSONObject();
			resp.put("tokenid", tokenid);
			resp.put("data", tokdata.to0xString());
			ret.put("response", resp);
		
		}else if(action.equals("import")) {
			
			String data 		= getParam("data");
			MiniData tokendata 	= new MiniData(data);
			Token newtok 		= Token.convertMiniDataVersion(tokendata);
			
			//Add this..
			TxPoWSearcher.importToken(newtok);
			
			JSONObject resp = new JSONObject();
			resp.put("token", newtok.toJSON());
			ret.put("response", resp);
			
		}else {
			
			if(tokenid.equals("")) {
				
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
			
				//Get ALL the tokens in the chain..
				ArrayList<Token> alltokens = TxPoWSearcher.getAllTokens();
				
				for(Token tok : alltokens) {
					//Add to our list
					toksarr.add(tok.toJSON());
				}
				
				ret.put("response", toksarr);
			
			}else {
				
				//Search for one token..
				Token tok = TxPoWSearcher.getToken(new MiniData(tokenid));
				if(tok == null) {
					throw new CommandException("Token not found : "+tokenid);
				}
				ret.put("response", tok.toJSON());
				
			}
		}
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new tokens();
	}

}
