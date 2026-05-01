package org.minima.system.commands.base;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.Crypto;
import org.minima.utils.MiniFile;
import org.minima.utils.json.JSONObject;
import org.minima.utils.sphincs.SPHINCS;
import org.minima.utils.sphincs.SPHINCSSignature;

public class sphincs extends Command {

	public sphincs() {
		super("sphincs","[action:] (seed:) (data:) (privatekey:) (publickey:) (file:) - SPHINCS signature scheme functionality");
	}
	
	@Override
	public String getFullHelp() {
		return "\nsphincs\n"
				+ "\n"
				+ "SPHINCS+ generate, sign and verify.\n"
				+ "\n"
				+ "Generates a SPHINCS+ key signs messages and verifies.\n"
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
		return new ArrayList<>(Arrays.asList(new String[]{"action","seed", "data","privatekey",
				"file","publickey","signature","amount","address","tokenid"}));
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
			resp.put("address", sphincs.getSPHINCSAddress().getMinimaAddress());
			resp.put("script", sphincs.getKISSVMScript());
			resp.put("publickey", sphincs.getPublicKey().to0xString());
			resp.put("privatekey", sphincs.getPrivateKey().to0xString());
		
		}else if(action.equals("sign")) {
			
			MiniData privatekey = getDataParam("privatekey");
			MiniData message 	= getDataParam("data");
			
			SPHINCS sphincs = new SPHINCS();
			sphincs.initPrivateKey(privatekey);
			
			SPHINCSSignature sig = sphincs.signMessage(message);
			
			if(existsParam("file")) {
				String file = getParam("file");
				
				//Create the file
				File backupfile = MiniFile.createBaseFile(file);
				if(backupfile.exists()) {
					backupfile.delete();
				}
				
				MiniFile.writeObjectToFile(backupfile, sig);
				
				resp.put("signaturefile", backupfile.getAbsolutePath());
				resp.put("size", backupfile.length());
				
			}else {
				MiniData sigdata = MiniData.getMiniDataVersion(sig);
				resp.put("signature", sigdata.to0xString());
			}
		
		}else if(action.equals("verify")) {
			
			MiniData publickey = getDataParam("publickey");
			MiniData message = getDataParam("data");
			
			SPHINCSSignature sig = null;
			if(existsParam("file")) {
				String file = getParam("file");
				File ff = MiniFile.createBaseFile(file);
				if(!ff.exists()) {
					throw new CommandException("File does not exist : "+ff.getAbsolutePath());
				}
				
				//Load it in..
				byte[] txndata = MiniFile.readCompleteFile(ff);
				
				//Convert to MiniData
				MiniData minitxn = new MiniData(txndata);
				sig = SPHINCSSignature.convertMiniDataVersion(minitxn);	
				
			}else {
				MiniData minitxn = getDataParam("signature");
				sig = SPHINCSSignature.convertMiniDataVersion(minitxn);
			}
			
			//NOW verify..
			boolean valid = SPHINCS.verifySignature(message, sig, publickey);
			resp.put("valid", valid);
		
		}else if(action.equals("transaction")) {
			
			//Details..
			MiniNumber amount 	= getNumberParam("amount");
			String address 		= getAddressParam("address");
			MiniData tokenid	= getDataParam("tokenid");
			
			//Now construct a RAW txn..
			
			
			
		}else if(action.equals("test")) {
			
			//Get the string seed
			String strseed = "xxx";
			MiniData data = new MiniData("0x00");
			
			//HASH the seed
			MiniData seed = new MiniData(Crypto.getInstance().hashData(strseed.getBytes()));
			
			//Generate a SPHINCS key
			SPHINCS sphincs = new SPHINCS(seed);
			
			SPHINCSSignature sig = sphincs.signMessage(data);
			
			/*//Write to file..
			File backupfile = MiniFile.createBaseFile("sphincs.dat");
			if(backupfile.exists()) {
				backupfile.delete();
			}
			
			MiniFile.writeObjectToFile(backupfile, sig);
			
			//READ data
			byte[] txndata = MiniFile.readCompleteFile(backupfile);
			
			//Convert to MiniData
			MiniData minitxn = new MiniData(txndata);
			MinimaLogger.log("File read size : "+minitxn.getLength());
			*/
			
			MiniData minitxn = MiniData.getMiniDataVersion(sig);
			SPHINCSSignature sigfile = SPHINCSSignature.convertMiniDataVersion(minitxn);
			
			boolean verify = sphincs.verifySignature(data, sigfile, sphincs.getPublicKey());
			resp.put("verify", verify);
			
			
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
