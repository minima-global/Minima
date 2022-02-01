package org.minima.system.commands.base;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.Crypto;
import org.minima.utils.json.JSONObject;

public class hash extends Command {

	public hash() {
		super("hash","[data:] (type:keccak|sha2|sha3)- Hash the data - default KECCAK");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		String datastr = getParam("data");
		
		MiniData data = null;
		if(datastr.startsWith("0x")) {
			data = new MiniData(datastr);
		}else {
			data = new MiniData(new MiniString(datastr).getData());
		}
		
		String hashtype = getParam("type", "keccak");
	
		byte[] hash = null;
		if(hashtype.equals("keccak")) {
			hash = Crypto.getInstance().hashData(data.getBytes());
		
		}else if(hashtype.equals("sha2")) {
			hash = Crypto.getInstance().hashSHA2(data.getBytes());
		
		}else if(hashtype.equals("sha3")) {
			hash = Crypto.getInstance().hashSHA3(data.getBytes());
		
		}else {
			throw new CommandException("Invalid hash type : "+hashtype);
		}
		
		JSONObject resp = new JSONObject();
		resp.put("input", datastr);
		resp.put("data", data.to0xString());
		resp.put("type", hashtype);
		resp.put("hash", new MiniData(hash).to0xString());
		
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new hash();
	}

}
