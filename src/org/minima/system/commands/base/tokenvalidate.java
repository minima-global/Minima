package org.minima.system.commands.base;

import java.util.ArrayList;

import org.minima.objects.Token;
import org.minima.objects.base.MiniData;
import org.minima.objects.keys.Signature;
import org.minima.objects.keys.TreeKey;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.RPCClient;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;

public class tokenvalidate extends Command {

	public tokenvalidate() {
		super("tokenvalidate","[tokenid:] - validate the signature and web link in a token");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
	
		//Get the tokenid
		String tokenid = getParam("tokenid");
		
		//Is it Minima..
		if(tokenid.equals("0x00")) {
			throw new CommandException("The Main Minima Token 0x00 has no signature");
		}
		
		//Get ALL the tokens in the chain..
		ArrayList<Token> alltokens = TxPoWSearcher.getAllTokens();
		
		//Now get the correct token
		Token tok = null;
		for(Token token : alltokens) {
			if(token.getTokenID().to0xString().equals(tokenid)) {
				tok = token;
				break;
			}
		}
		
		//Did we find it
		if(tok == null) {
			throw new CommandException("Token not found : "+tokenid);
		}
			
		//Now validate..
		JSONObject description = (JSONObject) new JSONParser().parse(tok.getName().toString());
		
		JSONObject resp = new JSONObject();
		
		//Does it have a signature..
		if(description.containsKey("signature") && description.containsKey("signedby")) {
			
			JSONObject sign = new JSONObject();
			sign.put("signed", true);
			
			//get the signature
			String sigstr 		= (String) description.get("signature");
			MiniData sigdata 	= new MiniData(sigstr);
			Signature sig 		= Signature.convertMiniDataVersion(sigdata);
			MiniData root 		= sig.getRootPublicKey(); 
			
			//Who signed it..
			String signedby = (String) description.get("signedby");
			MiniData pubkey = new MiniData(signedby); 
			if(!pubkey.isEqual(root)) {
				sign.put("valid", false);
				sign.put("reason", "Public key does not match signedby : "+root);
				
			}else {
				sign.put("signedby", pubkey.to0xString());
				
				//Get the coinid of the token
				MiniData coinid = tok.getCoinID();
				
				//Verify the signature..
				TreeKey tk = new TreeKey();
				tk.setPublicKey(sig.getRootPublicKey());
				
				//Check the signature
				boolean valid = tk.verify(coinid, sig);
				sign.put("valid", valid);
				if(!valid) {
					sign.put("reason", "Signature fails");
				}
			}
			
			resp.put("signature", sign);
			
		}else {
			
			JSONObject sign = new JSONObject();
			sign.put("signed", false);
			resp.put("signature", sign);
		}
		
		//Does it have a web URL
		if(description.containsKey("webvalidate")) {
			
			JSONObject wval = new JSONObject();
			wval.put("webvalidate", true);
			
			//Get the URL
			String url = (String) description.get("webvalidate");
			wval.put("url", url);
			
			//Now load that file..
			try {
				//Get the value
				String val = RPCClient.sendGET(url).trim();
				
				//Do they match
				if(val.equals(tok.getTokenID().to0xString())) {
					wval.put("valid", true);
				}else {
					wval.put("valid", false);
					wval.put("reason", "Data in file does not match tokenid : "+val);
				}
				
			}catch(Exception exc) {
				//Something gone wrong
				wval.put("valid", false);
				wval.put("reason", "Could not download web file : "+exc.toString());
			}
			
			resp.put("web", wval);
		
		}else {
			
			JSONObject wval = new JSONObject();
			wval.put("webvalidate", false);
			resp.put("web", wval);
		}
		
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new tokenvalidate();
	}

}
