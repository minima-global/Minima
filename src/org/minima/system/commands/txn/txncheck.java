package org.minima.system.commands.txn;

import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.objects.Coin;
import org.minima.objects.Token;
import org.minima.objects.Transaction;
import org.minima.objects.TxPoW;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.brains.TxPoWChecker;
import org.minima.system.brains.TxPoWGenerator;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class txncheck extends Command {

	public txncheck() {
		super("txncheck","[id:] - Show details about the transaction");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();

		TxnDB db  = MinimaDB.getDB().getCustomTxnDB();
		
		String id = getParam("id");
		
		//Get the Transaction..
		TxnRow txnrow 	= db.getTransactionRow(getParam("id"));
		if(txnrow == null) {
			throw new CommandException("Transaction not found : "+id);
		}
		
		Transaction txn = txnrow.getTransaction();
		Witness wit 	= txnrow.getWitness();
		
		//Inputs and outputs
		ArrayList<Coin> inputs = txnrow.getTransaction().getAllInputs();
		ArrayList<Coin> outputs = txnrow.getTransaction().getAllOutputs();
				
		//The results
		JSONObject details = new JSONObject();
		
		//First get a list of all the Output tokens..
		ArrayList<String> tokens = new ArrayList<>();
		for(Coin cc : inputs) {
			MiniData tokenhash = cc.getTokenID();
			if(tokenhash.isEqual(Token.TOKENID_CREATE)){
				tokenhash = Token.TOKENID_MINIMA;
			}
			
			String tok = tokenhash.to0xString();
			if(!tokens.contains(tok)) {
				tokens.add(tok);	
			}
		}
		
		for(Coin cc : outputs) {
			MiniData tokenhash = cc.getTokenID();
			if(tokenhash.isEqual(Token.TOKENID_CREATE)){
				tokenhash = Token.TOKENID_MINIMA;
			}
			
			String tok = tokenhash.to0xString();
			if(!tokens.contains(tok)) {
				tokens.add(tok);	
			}
		}
		
		//Now cycle through and check there is enough inputs..
		JSONArray alltokens = new JSONArray();
		for(String token : tokens) {
			MiniData tok = new MiniData(token);
			
			//The output total amount
			MiniNumber outamt = txn.sumOutputs(tok);
			
			//The input total amount
			MiniNumber inamt = txn.sumInputs(tok);
			
			//Add to thew details..
			JSONObject tokcoin = new JSONObject();
			tokcoin.put("tokenid", token);
			tokcoin.put("input", inamt.toString());
			tokcoin.put("output", outamt.toString());
			tokcoin.put("difference", inamt.sub(outamt).toString());
			alltokens.add(tokcoin);
		}
		details.put("coins", alltokens);
		
		//Get some details
		details.put("tokens", tokens.size());
		details.put("inputs", inputs.size());
		MiniNumber totminimain = MiniNumber.ZERO;
		for(Coin cc: inputs) {
			totminimain = totminimain.add(cc.getAmount());
		}
		
		details.put("outputs", outputs.size());
		MiniNumber totminimaout = MiniNumber.ZERO;
		for(Coin cc: outputs) {
			totminimaout = totminimaout.add(cc.getAmount());
		}
		
		MiniNumber diff = totminimain.sub(totminimaout);
		
		details.put("burn", diff.toString());
		details.put("validamounts", txnrow.getTransaction().checkValid());
		
		int sigs = txnrow.getWitness().getAllSignatures().size();
		details.put("signatures", sigs);
		
		//Now some low level checks..
		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
		
		//Create a TxPoW..
		TxPoW temp = TxPoWGenerator.generateTxPoW(txn, wit);
		
		//Redo any checks..
		txn.clearIsMonotonic();
		
		boolean validbasic 		= TxPoWChecker.checkTxPoWBasic(temp); 
		boolean validsig 		= TxPoWChecker.checkSignatures(temp); 
		boolean validmmr 		= TxPoWChecker.checkMMR(tip.getMMR(), temp);
		boolean validscripts 	= TxPoWChecker.checkTxPoWScripts(tip.getMMR(), temp, tip.getTxPoW());

		JSONObject valid = new JSONObject();
		valid.put("basic", validbasic);
		valid.put("signatures", validsig);
		valid.put("mmrproofs", validmmr);
		valid.put("scripts", validscripts);
		
		details.put("valid", valid);
		
		
		JSONObject resp = new JSONObject();
		ret.put("response", details);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new txncheck();
	}

}
