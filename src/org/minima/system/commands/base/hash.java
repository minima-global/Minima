package org.minima.system.commands.base;

import org.minima.objects.base.MiniData;
import org.minima.system.commands.Command;
import org.minima.utils.Crypto;
import org.minima.utils.json.JSONObject;

public class hash extends Command {

	public hash() {
		super("hash","[data:] - Hash the data using KECCAK 256");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		MiniData data = getDataParam("data"); 
		
		byte[] hash = Crypto.getInstance().hashData(data.getBytes());
		
		
		ret.put("response", new MiniData(hash).to0xString());
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new hash();
	}

}
