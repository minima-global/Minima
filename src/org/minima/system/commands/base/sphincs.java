package org.minima.system.commands.base;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.Crypto;
import org.minima.utils.MiniFile;
import org.minima.utils.json.JSONObject;
import org.minima.utils.sphincs.SPHINCS;

public class sphincs extends Command {

	public sphincs() {
		super("sphincs","(action:) - SPHINCS signature scheme functionality");
	}
	
	@Override
	public String getFullHelp() {
		return "\nsphincs\n"
				+ "\n"
				+ "Hash the data or file - default SHA3.\n"
				+ "\n"
				+ "Returns the hash of the data provided using the algorithm specified.\n"
				+ "\n"
				+ "data:\n"
				+ "    The data to hash. Can be HEX (0x) or a string in quotes.\n"
				+ "    String data will return the the byte representation of the string.\n"
				+ "\n"
				+ "file:\n"
				+ "    The file path - can be the full path or relative to your base folder\n"
				+ "\n"
				+ "type: (optional)\n"
				+ "    sha2 or sha3. The hashing algorithm to use, default is SHA3.\n"
				+ "    BTC and ETH support sha2 or sha3.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "hash data:0x1C8AFF950685C2ED4BC3174F3472287B56D9517B9C948127319A09A7A36DEAC8\n"
				+ "\n"
				+ "hash file:myfile.txt\n"
				+ "\n"
				+ "hash data:\"this is my secret\" type:sha2\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","seed", "data"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		JSONObject resp = new JSONObject();
		
		String action=getParam("action");
		
		if(action.equals("generate")) {
			
			//Get the string seed
			String strseed = getParam("seed");
			
			//HASH the seed
			MiniData seed = new MiniData(Crypto.getInstance().hashData(strseed.getBytes()));
			
			//Generate a SPHINCS key
			SPHINCS sphincs = new SPHINCS(seed);
			
			//Get the public key
			resp.put("publickey", sphincs.getPublicKey().to0xString());
			resp.put("privatekey", sphincs.getPrivateKey().to0xString());
		
		}else if(action.equals("sign")) {
			
			MiniData message = getDataParam("data");
			
			
			
			
		}else {
			throw new CommandException("undefined action : "+action);
		}
		
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new sphincs();
	}

}
