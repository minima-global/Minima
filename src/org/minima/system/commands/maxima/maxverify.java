package org.minima.system.commands.maxima;

import org.minima.objects.base.MiniData;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.network.maxima.MaximaManager;
import org.minima.utils.encrypt.SignVerify;
import org.minima.utils.json.JSONObject;

public class maxverify extends Command {

	public maxverify() {
		super("maxverify","[data:] [publickey:] [signature:] - Verify data with a Maxima Public key");
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
