package org.minima.system.commands.maxima;

import java.security.KeyPair;

import org.minima.objects.base.MiniData;
import org.minima.system.commands.Command;
import org.minima.utils.encrypt.GenerateKey;
import org.minima.utils.json.JSONObject;

public class maxcreate extends Command {

	public maxcreate() {
		super("maxcreate","Create a 128bit Public and Private RSA key pair. Can use with maxsign and maxverify.");
	}
	
	@Override
	public String getFullHelp() {
		return "\nmaxcreate\n"
				+ "\n"
				+ "Create a 128 bit RSA public and private key. You can use them with maxsign and maxverify.\n"
				+ "\n"
				+ "Returns the public amd private key HEX data.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "maxcreate\n";
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		//Create a new new maxima ident..
		KeyPair generateKeyPair = GenerateKey.generateKeyPair();
		
		byte[] publicKey 	= generateKeyPair.getPublic().getEncoded();
		MiniData pubk 		= new MiniData(publicKey);
		
		byte[] privateKey	= generateKeyPair.getPrivate().getEncoded();
		MiniData privk 		= new MiniData(privateKey);
		
		JSONObject resp = new JSONObject();
		resp.put("publickey", pubk.to0xString());
		resp.put("privatekey", privk.to0xString());
		
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new maxcreate();
	}

}
