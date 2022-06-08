package org.minima.system.commands.signatures;

import org.minima.objects.base.MiniData;
import org.minima.objects.keys.Signature;
import org.minima.objects.keys.TreeKey;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.json.JSONObject;

public class verify extends Command {

	public verify() {
		super("verify","[publickey:] [data:] [signature:] - Verify a signature");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		MiniData data 		= getDataParam("data");
		MiniData pubk 		= getDataParam("publickey");
		MiniData signature 	= getDataParam("signature");
		
		//Create the Signature
		Signature sig 		= Signature.convertMiniDataVersion(signature);
		MiniData sigpubk 	= sig.getRootPublicKey(); 
		
		//Check the Public key..
		if(!pubk.isEqual(sigpubk)) {
			throw new CommandException("Signature publickey is different : "+sigpubk.to0xString());
		}
		
		//Create a signature scheme checker..
		TreeKey tk = new TreeKey();
		tk.setPublicKey(sig.getRootPublicKey());
		
		//And check the data..
		boolean valid = tk.verify(data, sig);
		
		if(!valid) {

			ret.put("status", false);
			ret.put("message", "Signature NOT valid");
		}else {
			ret.put("response", "Signature valid");	
		}
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new verify();
	}

}
