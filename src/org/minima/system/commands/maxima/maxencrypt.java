package org.minima.system.commands.maxima;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.network.maxima.MaximaManager;
import org.minima.utils.encrypt.CryptoPackage;
import org.minima.utils.encrypt.SignVerify;
import org.minima.utils.json.JSONObject;

public class maxencrypt extends Command {

	public maxencrypt() {
		super("maxencrypt","[data:] [publickey:] - Encrypt data using a public RSA key");
	}
	
	/*@Override
	public String getFullHelp() {
		return "\nmaxsign\n"
				+ "\n"
				+ "Sign a piece of data with your Maxima ID.\n"
				+ "\n"
				+ "Returns the signature of the data, signed with your Maxima private key or the specified key.\n"
				+ "\n"
				+ "data:\n"
				+ "    The 0x HEX data to sign.\n"
				+ "\n"
				+ "privatekey: (optional)\n"
				+ "    The 0x HEX data of the private key from maxcreate. Uses your Maxima ID otherwise.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "maxsign data:0xCD34..\n"
				+ "\n"
				+ "maxsign data:0xCD34.. privatekey:0x30819..\n";
	}*/
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"data","publickey"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		//Is it a JSON
		String datastr = null;
		if(isParamJSONObject("data")) {
			datastr = getJSONObjectParam("data").toString();
		}else if(isParamJSONArray("data")) {
			datastr = getJSONArrayParam("data").toString();
		}else {
			datastr = getParam("data");
		}
		
		MiniData data = null;
		if(datastr.startsWith("0x")) {
			data = new MiniData(datastr);
		}else {
			data = new MiniData(new MiniString(datastr).getData());
		}
		
		MiniData publickey = getDataParam("publickey");
		
		//Encrypt the data..
		CryptoPackage cp = new CryptoPackage();
		cp.encrypt(data.getBytes(), publickey.getBytes());
		
		//byte[] sigBytes = SignVerify.sign(priv.getBytes(), data.getBytes());
		//MiniData sign = new MiniData(sigBytes);
		
		JSONObject resp = new JSONObject();
		
		resp.put("encrypted", cp.getCompleteEncryptedData().to0xString());
		
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new maxencrypt();
	}

}
