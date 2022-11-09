package org.minima.system.commands.maxima;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.objects.base.MiniData;
import org.minima.system.commands.Command;
import org.minima.utils.encrypt.SignVerify;
import org.minima.utils.json.JSONObject;

public class maxverify extends Command {

	public maxverify() {
		super("maxverify","[data:] [publickey:] [signature:] - Verify data with a Maxima Public key");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"data","publickey","signature"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		MiniData data = getDataParam("data");
		MiniData pubk = getDataParam("publickey");
		MiniData sign = getDataParam("signature");
		
		boolean valid = SignVerify.verify(pubk.getBytes(), data.getBytes(), sign.getBytes());
		
		JSONObject resp = new JSONObject();
		
		resp.put("valid", valid);
		
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new maxverify();
	}

}
