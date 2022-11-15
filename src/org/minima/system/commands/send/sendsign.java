package org.minima.system.commands.send;

import java.io.File;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.wallet.ScriptRow;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Coin;
import org.minima.objects.ScriptProof;
import org.minima.objects.Transaction;
import org.minima.objects.TxPoW;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.keys.Signature;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.MiniFile;
import org.minima.utils.json.JSONObject;

public class sendsign extends Command {

	public sendsign() {
		super("sendsign","[file:] - Sign a previously created txn");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"file"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
	
		String filename = getParam("file");
		
		//Load the data
		byte[] data = MiniFile.readCompleteFile(MiniFile.createBaseFile(filename));
		
		//Create the MiniData
		MiniData sendtxpow = new MiniData(data); 
		
		//Now convert back into a TxPoW
		TxPoW txp = TxPoW.convertMiniDataVersion(sendtxpow);
		
		//Get the main Wallet
		Wallet walletdb = MinimaDB.getDB().getWallet();
		
		//Create a list of the required signatures
		ArrayList<String> reqsigs = new ArrayList<>();
		
		//Get the sigs required.. for the main transaction
		Transaction trans 	= txp.getTransaction();
		Witness witness 	= txp.getWitness();
		ArrayList<Coin> inputs = trans.getAllInputs();
		for(Coin input : inputs) {
		
			//Add the script proofs
			String scraddress 	= input.getAddress().to0xString();
			
			//Get the ScriptRow..
			ScriptRow srow = walletdb.getScriptFromAddress(scraddress);
			if(srow == null) {
				throw new CommandException("SERIOUS ERROR script missing for simple address : "+scraddress);
			}
			ScriptProof pscr = new ScriptProof(srow.getScript());
			witness.addScript(pscr);
			
			//Add this address / public key to the list we need to sign as..
			String pubkey = srow.getPublicKey();
			if(!reqsigs.contains(pubkey)) {
				//Use the wallet..
				Signature signature = walletdb.signData(pubkey, trans.getTransactionID());
				
				//Add it..
				witness.addSignature(signature);
			}	
		}
		
		//Get the sigs required.. for the BURN transaction
		trans 	= txp.getBurnTransaction();
		witness = txp.getBurnWitness();
		inputs 	= trans.getAllInputs();
		for(Coin input : inputs) {
		
			//Add the script proofs
			String scraddress 	= input.getAddress().to0xString();
			
			//Get the ScriptRow..
			ScriptRow srow = walletdb.getScriptFromAddress(scraddress);
			if(srow == null) {
				throw new CommandException("SERIOUS ERROR script missing for simple address : "+scraddress);
			}
			ScriptProof pscr = new ScriptProof(srow.getScript());
			witness.addScript(pscr);
			
			//Add this address / public key to the list we need to sign as..
			String pubkey = srow.getPublicKey();
			if(!reqsigs.contains(pubkey)) {
				//Use the wallet..
				Signature signature = walletdb.signData(pubkey, trans.getTransactionID());
				
				//Add it..
				witness.addSignature(signature);
			}	
		}
		
		//Create the file..
		File txnfile = MiniFile.createBaseFile("signedtransaction-"+System.currentTimeMillis()+".txn");
		
		//Write it to a file..
		MiniFile.writeObjectToFile(txnfile, txp);
		
		JSONObject sigtran = new JSONObject();
		sigtran.put("txpow", txnfile.getAbsolutePath());
		
		JSONObject resp = new JSONObject();
		ret.put("response", sigtran);
	
		return ret;
	}
	
	@Override
	public Command getFunction() {
		return new sendsign();
	}

	public static void main(String[] zArgs) {
		
		for(int i=0;i<512;i++) {
			
			MiniData data = new MiniData(new BigInteger(Integer.toString(i)));
			
			System.out.println(data.to0xString());
			
		}
		
		
		
	}
}