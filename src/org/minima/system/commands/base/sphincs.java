package org.minima.system.commands.base;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMRData;
import org.minima.database.mmr.MMRProof;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.objects.Coin;
import org.minima.objects.StateVariable;
import org.minima.objects.Transaction;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.system.brains.TxPoWGenerator;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.commands.CommandRunner;
import org.minima.utils.Crypto;
import org.minima.utils.MiniFile;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.sphincs.SPHINCS;
import org.minima.utils.sphincs.SPHINCSSignature;
import org.minima.utils.sphincs.FORS.FORSSignature;

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
			MiniData privatekey	= getDataParam("privatekey");
			
			//Generate the SPHINCS key
			SPHINCS sphincs = new SPHINCS();
			sphincs.initPrivateKey(privatekey);
			
			//ID of the custom transaction
			String randomid 	= MiniData.getRandomData(32).to0xString();
			
			//Now construct the transaction..
			JSONObject result = runCommand("txncreate id:"+randomid);
			
			String command 	= "txnaddamount id:"+randomid+" fromaddress:"+sphincs.getSPHINCSAddress().getMinimaAddress()
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
			
			ArrayList<Coin> inputcoins 	= transaction.getAllInputs();
			ArrayList<Coin> outputcoins = transaction.getAllOutputs();
			
			/**
			 * Calculate the message
			 */
			int totin = inputcoins.size();
			if(totin>8) {
				
				//Delete transaction
				runCommand("txndelete id:"+randomid);
				
				throw new CommandException("Input Coin number too great.. MAX 8. Pls Send a smaller amount.");
			}
			
			String instring = inputcoins.get(0).getCoinID().to0xString();
			for(int i=1;i<totin;i++) {
				Coin cc 	= inputcoins.get(i);
				instring	= instring+cc.getCoinID().to0xString();
			}

			int totout = outputcoins.size();
			String outstring = "";
			for(int i=0;i<totout;i++) {
				Coin cc 	= outputcoins.get(i);
				outstring	= outstring+getOutCoinString(cc);
			}
			
			//Now create the complete message string..
			String fullstring	= totin+"SPHINCS"+totout+"SPHINCS"+instring+"COINJOIN"+outstring;
			
			//Now the full message
			MiniData message	= new MiniData(new MiniString(fullstring).getData());
			
			//Now SIGN the message
			SPHINCSSignature sig = sphincs.signMessage(message);
			
			/**
			 * Add the state Vars
			 */
			//The Minima Sig
			MiniData minisig = MiniData.getMiniDataVersion(sig.getWOTSSignature()); 
			StateVariable svminimasig 	= new StateVariable(100, minisig.to0xString());
			transaction.addStateVariable(svminimasig);
			
			//The FORS root
			StateVariable svforsroot 	= new StateVariable(101, sig.getFORSRoot().getData().to0xString());
			transaction.addStateVariable(svforsroot);
			
			//HORST trees
			FORSSignature forssignature = sig.getFORSSignature();
			for(int i=0;i<16;i++) {
				
				//The base state pos
				int statepos = i*5;
				
				//The HORST root
				MMRData horstroot 		= forssignature.getHORSTRoots().get(i);
				StateVariable svhorstroot	= new StateVariable(statepos, horstroot.getData().to0xString());
				transaction.addStateVariable(svhorstroot);
				
				//The HORST root proof
				MMRProof horstproof			= forssignature.getHORSTTreeProofs().get(i);
				MiniData horstproofdata		= MiniData.getMiniDataVersion(horstproof);
				StateVariable svhorstproof	= new StateVariable(statepos+1, horstproofdata.to0xString());
				transaction.addStateVariable(svhorstproof);
				
				//The SIG value (private key preimage of public key)
				MiniData privkeyval = forssignature.getHORSTSignature().getSignatureValues().get(i);
				StateVariable svprivkeyval	= new StateVariable(statepos+2, privkeyval.to0xString());
				transaction.addStateVariable(svprivkeyval);
				
				//The Public key root proof
				MMRProof privkeyproof		= forssignature.getHORSTSignature().getPublicKeyTreeProofs().get(i);
				MiniData privkeyproofdata	= MiniData.getMiniDataVersion(privkeyproof);
				StateVariable svsigvalproof	= new StateVariable(statepos+3, privkeyproofdata.to0xString());
				transaction.addStateVariable(svsigvalproof);
			}
			
			//Compute the correct CoinID
			TxPoWGenerator.precomputeTransactionCoinID(transaction);
					
			//Calculate transid
			transaction.calculateTransactionID();
			
			//Finally - Add the scripts..
			runCommand("txnscript id:"+randomid+" scripts:{\""+sphincs.getKISSVMScript()+"\":\"\"}");
			
			//Sort the MMR
			runCommand("txnmmr id:"+randomid);
			
			//And POST!
			result = runCommand("txnpost id:"+randomid+" mine:true");
			//result = runCommand("txnlist id:"+randomid);
			
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
		return zCoin.getAddress().to0xString()+"SPHINCS"+zCoin.getAmount().toString()+"SPHINCS"+zCoin.getTokenID().to0xString()+keepstate;
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
