package org.minima.system.commands.maxima;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.objects.base.MiniData;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.encrypt.CryptoPackage;
import org.minima.utils.json.JSONObject;

public class maxmessage extends Command {
	
	public maxmessage() {
		super("maxmessage","[action:] [data:] (encrypt:) (decrypt:) - Create an encrypted signed message.");
	}
	
	/*@Override
	public String getFullHelp() {
		return "\nmaxmessage\n"
				+ "\n"
				+ "Create a 128 bit RSA public and private key. You can use them with maxsign and maxverify.\n"
				+ "\n"
				+ "Returns the public amd private key HEX data.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "maxcreate\n";
	}*/
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","data","encrypt"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		JSONObject resp = new JSONObject();
		
		String action = getParam("action");
		
		if(action.equals("create")) {
			
			//Whats the message
			MiniData data = getDataParam("data");
			
			//Who is signing..
			MiniData pubkey 	= Main.getInstance().getMaxima().getPublicKey();
			MiniData privkey 	= Main.getInstance().getMaxima().getPrivateKey();
			
			//Now create a Max Message Object
			MaximumMessage mm = new MaximumMessage(data);
			mm.createSignature(pubkey, privkey);
			
			//Get the MiniData version
			MiniData mdata = mm.createMiniDataVersion();
			
			resp.put("message", mm.toJSON());
			
			//Now create an encrypted version
			if(existsParam("encrypt")) {
				MiniData encrypt = getDataParam("encrypt");
				
				CryptoPackage cp = new CryptoPackage();
				cp.encrypt(mdata.getBytes(), encrypt.getBytes());
				MiniData encdata = cp.getCompleteEncryptedData();
				
				resp.put("encrypted", true);
				resp.put("data", encdata.to0xString());
			}else {
				resp.put("encrypted", false);
				resp.put("data", mdata.to0xString());
			}
			
		}else if(action.equals("check")) {
			
			MiniData data = getDataParam("data");
			
			//First see if it is NOT encrypted
			try {
				//If this works..
				MaximumMessage mm = MaximumMessage.ConvertMiniDataVersion(data);
				
				//Check the signature
				boolean validsig = mm.checkSignature();
				
				//Print it out..
				resp.put("encrypted", false);
				resp.put("message", mm.toJSON());
				
			}catch(Exception exc) {
				
				//Ok - try and decrypt it..
				MiniData privkey = Main.getInstance().getMaxima().getPrivateKey();
				
				try {
					CryptoPackage cp = new CryptoPackage();
					cp.ConvertMiniDataVersion(data);
					
					byte[] decdata = cp.decrypt(privkey.getBytes());
					
					MaximumMessage mm = MaximumMessage.ConvertMiniDataVersion(new MiniData(decdata));
					resp.put("encrypted", true);
					resp.put("message", mm.toJSON());
					
				}catch(Exception decexc){
					throw new CommandException("Invalid message..cannot decrypt");
				}
			}
			
		}else {
			throw new CommandException("Invalid action : "+action);
		}
		
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new maxmessage();
	}
}
