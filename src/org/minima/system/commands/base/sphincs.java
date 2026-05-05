package org.minima.system.commands.base;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.Transaction;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.commands.CommandRunner;
import org.minima.utils.Crypto;
import org.minima.utils.MiniFile;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.sphincs.SPHINCS;
import org.minima.utils.sphincs.SPHINCSSignature;
import org.minima.utils.sphincs.SPHINCSUtils;

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
				+ "action:\n"
				+ "    generate - generate a sphincs key.\n"
				+ "    sign - sign a piece of data.\n"
				+ "    verify - sign a piece of data.\n"
				+ "    transaction - send from a certain private key.\n"
				+ "\n"
				+ "seed:\n"
				+ "    The seed used to generate a Key.\n"
				+ "\n"
				+ "data:\n"
				+ "    The data to sign / verify\n"
				+ "\n"
				+ "file:\n"
				+ "    The file path to save a signature to - can be the full path or relative to your base folder\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "sphincs action:generate seed:mysecretseed\n"
				+ "\n"
				+ "sphincs action:sign data:0x001122 privatekey:0x..\n"
				+ "\n"
				+ "sphincs action:verify data:0x001122 publickey:0x.. signature:0x..|file:thefile\n"
				+ "\n"
				+ "sphincs action:transaction privatekey:0x.. address:Mx.. tokenid:0x.. amount:10\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","seed", "data","privatekey",
				"file","publickey","signature","amount","address","tokenid","mine"}));
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
			
			//What is the KISSVM script
			String kissvmscript = SPHINCSUtils.getKISSVMScript(sphincs.getPublicKey());
			Address address  	= new Address(kissvmscript);
			
			//Get the public key
			resp.put("address", address.getMinimaAddress());
			resp.put("script", kissvmscript);
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
			MiniData privatekey	= getDataParam("privatekey");
			boolean mine		= getBooleanParam("mine",false);
			
			//Generate the SPHINCS key
			SPHINCS sphincs = new SPHINCS();
			sphincs.initPrivateKey(privatekey);
			
			//What is the address
			String sphincsscript 	= SPHINCSUtils.getKISSVMScript(sphincs.getPublicKey());
			Address sphincsaddress 	= new Address(sphincsscript);
			
			//ID of the custom transaction
			String randomid 	= MiniData.getRandomData(32).to0xString();
			
			//Now construct the transaction..
			JSONObject result = runCommand("txncreate id:"+randomid);
			
			String command 	= "txnaddamount id:"+randomid+" fromaddress:"+sphincsaddress.getMinimaAddress()
					+" address:"+address+" amount:"+amount+" tokenid:"+tokenid;
			
			result = runCommand(command);
			
			if(!(boolean)result.get("status")) {
				
				//Delete transaction
				runCommand("txndelete id:"+randomid);
				
				//Not enough funds!
				throw new CommandException(result.getString("error"));
			}
			
			//Get the transaction details
			TxnDB db 				= MinimaDB.getDB().getCustomTxnDB();
			TxnRow txnrow 			= db.getTransactionRow(randomid);
			Transaction transaction = txnrow.getTransaction();
			
			/**
			 * MAX 8 Input coins - or script runs out of operations as 1024 max
			 */
			if(transaction.getAllInputs().size()>8) {
				
				//Delete transaction
				runCommand("txndelete id:"+randomid);
				
				throw new CommandException("Input Coin number too great.. MAX 8. Pls Send a smaller amount.");
			}
			
			//Set all the outputs to NOT keep the state - is useless 20k
			ArrayList<Coin> outputcoins = transaction.getAllOutputs();
			for(int i=0;i<outputcoins.size();i++) {
				outputcoins.get(i).setStoreState(false);
			}
			
			//Get the TransactionID - to sign
			MiniData message	= SPHINCSUtils.calculateTransactionID(transaction);
			
			//Now SIGN the message
			SPHINCSSignature sig = sphincs.signMessage(message);
			
			//Add the state variables
			SPHINCSUtils.setupTransaction(transaction, sig);
			
			//Finally - Add the scripts..
			runCommand("txnscript id:"+randomid+" scripts:{\""+sphincsscript+"\":\"\"}");
			
			//Sort the MMR
			runCommand("txnmmr id:"+randomid);
			
			//And POST!
			result = runCommand("txnpost id:"+randomid+" mine:"+mine);
			
			//And delete..
			runCommand("txndelete id:"+randomid);
			
			//Check succeeded..
			if(!(boolean)result.get("status")) {
				//Didn't work..
				throw new CommandException((String)result.get("error"));
			}
			
			//And return..
			ret.put("response", result.get("response"));
			
			return ret;
			
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

	private String getOutCoinString(Coin zCoin) {
		String keepstate=(""+zCoin.storeState()).toUpperCase();
		return zCoin.getAddress().to0xString()+"SPHINCS"+zCoin.getTokenAmount().toString()+"SPHINCS"+zCoin.getTokenID().to0xString()+keepstate;
	}
	
	private JSONObject runCommand(String zCommand) {
		JSONArray res 		= CommandRunner.getRunner().runMultiCommand(zCommand);
		JSONObject result 	= (JSONObject) res.get(0);
		return result;
	}
	
	@Override
	public Command getFunction() {
		return new sphincs();
	}

}
