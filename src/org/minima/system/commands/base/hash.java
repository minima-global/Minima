package org.minima.system.commands.base;

import java.util.ArrayList;
import java.util.Arrays;

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
	public String getFullHelp() {
		return "\nhash\n"
				+ "\n"
				+ "Hash the data - default KECCAK.\n"
				+ "\n"
				+ "Returns the hash of the data provided using the algorithm specified.\n"
				+ "\n"
				+ "data:\n"
				+ "    The data to hash. Can be HEX (0x) or a string in quotes.\n"
				+ "    String data will return the the byte representation of the string.\n"
				+ "\n"
				+ "type: (optional)\n"
				+ "    keccak, sha2 or sha3. The hashing algorithm to use, default is KECCAK.\n"
				+ "    BTC and ETH support sha2 or sha3.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "hash data:0x1C8AFF950685C2ED4BC3174F3472287B56D9517B9C948127319A09A7A36DEAC8\n"
				+ "\n"
				+ "hash data:\"this is my secret\" type:sha2\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"data","type"}));
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
