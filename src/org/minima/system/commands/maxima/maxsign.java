package org.minima.system.commands.maxima;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.objects.base.MiniData;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.network.maxima.MaximaManager;
import org.minima.utils.encrypt.SignVerify;
import org.minima.utils.json.JSONObject;

public class maxsign extends Command {

	public maxsign() {
		super("maxsign","[data:] - Sign a piece of data with your Maxima ID");
	}
	
	@Override
	public String getFullHelp() {
		return "\nmaxsign\n"
				+ "\n"
				+ "Sign a piece of data with your Maxima ID.\n"
				+ "\n"
				+ "Returns the signature of the data, signed with your Maxima private key.\n"
				+ "\n"
				+ "data:\n"
				+ "    The 0x HEX data to sign.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "maxsign data:0xCD34..\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"data"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		MiniData data = getDataParam("data");
		
		MaximaManager max = Main.getInstance().getMaxima();
		
		//get the Private Key..
		MiniData priv = max.getPrivateKey();
		
		byte[] sigBytes = SignVerify.sign(priv.getBytes(), data.getBytes());
		MiniData sign = new MiniData(sigBytes);
		
		JSONObject resp = new JSONObject();
		
		resp.put("signature", sign.to0xString());
		
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new maxsign();
	}

}
