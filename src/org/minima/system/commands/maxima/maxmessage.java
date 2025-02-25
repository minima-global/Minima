package org.minima.system.commands.maxima;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.objects.base.MiniData;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.commands.base.newaddress;
import org.minima.utils.encrypt.CryptoPackage;
import org.minima.utils.json.JSONObject;

public class maxmessage extends Command {
	
	public maxmessage() {
		super("maxmessage","[action:] [data:] (publickey:) (privatekey:) - Create an encrypted signed message.");
	}
	
	@Override
	public String getFullHelp() {
		return "\nmaxmessage\n"
				+ "\n"
				+ "Create a signed encrypted (or unencrypted) message.\n"
				+ "\n"
				+ "Use Your Maxima public / private keys by default or create new keys with maxcreate.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "maxmessage action:encrypt data:0xFFEEDD publickey:0x/Mx..30819F300D0..\n"
				+ "\n"
				+ "maxmessage action:decrypt data:0xTHE_DATA\n"
				+ "\n"
				+ "maxmessage action:decrypt data:0xTHE_DATA privatekey:0x..PrivateKey\n"
				+ "";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","data","publickey","privatekey"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		JSONObject resp = new JSONObject();
		
		String action = getParam("action");
		
		if(action.equals("encrypt")) {
			
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
			MiniData encryptpublickey = getDataParam("publickey", MiniData.ZERO_TXPOWID);
			
			//Is it unencrypted
			if(encryptpublickey.isEqual(MiniData.ZERO_TXPOWID)) {
				
				//Concatenate..
				MiniData total = new MiniData("0xFFFFFFFF").concat(mdata);
				
				resp.put("encrypted", false);
				resp.put("data", total.to0xString());
				
			}else {
				
				//Try and encrypt the message
				try {
					
					CryptoPackage cp = new CryptoPackage();
					cp.encrypt(mdata.getBytes(), encryptpublickey.getBytes());
					MiniData encdata = cp.getCompleteEncryptedData();
					
					resp.put("encrypted", true);
					resp.put("data", encdata.to0xString());
					
				}catch(Exception exc) {
					//Something went wrong - prob invalid key
					throw new CommandException("Invalid Publickey..cannot encrypt");
				}
			}
			
		}else if(action.equals("decrypt")) {
			
			//Get the Data
			MiniData data = getDataParam("data");
			
			try {
				
				//Is it un-encrypted..
				String datstr = data.to0xString();
				if(datstr.startsWith("0xFFFFFFFF")) {
					
					//Get the actual DATA - remove the 0xFFFFFFFF
					MiniData subdata = new MiniData(datstr.substring(10));
				
					//If this works..
					MaximumMessage mm = MaximumMessage.ConvertMiniDataVersion(subdata);
					
					//Check the signature
					boolean validsig = mm.checkSignature();
					
					//Print it out..
					resp.put("encrypted", false);
					resp.put("message", mm.toJSON());
					
				}else {
					
					//Ok - try and decrypt it.. default to your Maxima Key
					MiniData privkey = getDataParam("privatekey",Main.getInstance().getMaxima().getPrivateKey());
					
					CryptoPackage cp = new CryptoPackage();
					cp.ConvertMiniDataVersion(data);
					
					byte[] decdata = cp.decrypt(privkey.getBytes());
					
					MaximumMessage mm = MaximumMessage.ConvertMiniDataVersion(new MiniData(decdata));
					
					resp.put("encrypted", true);
					resp.put("message", mm.toJSON());
				}
				
			}catch(Exception decexc){
				throw new CommandException("Invalid message..cannot decrypt");
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
