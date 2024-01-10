package org.minima.system.commands.txn;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMRProof;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.objects.Coin;
import org.minima.objects.CoinProof;
import org.minima.objects.ScriptProof;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.params.GlobalParams;
import org.minima.utils.json.JSONObject;

public class txnmmr extends Command {

	public txnmmr() {
		super("txnmmr","[id:] - Add all MMR proofs to a transaction");
	}
	
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"id"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();

		TxnDB db = MinimaDB.getDB().getCustomTxnDB();
		
		//The transaction
		String id 			= getParam("id");
		
		//Get the Transaction
		TxnRow txnrow 	= db.getTransactionRow(getParam("id"));
		if(txnrow == null) {
			throw new CommandException("Transaction not found : "+id);
		}
		
		Transaction trans 	= txnrow.getTransaction();
		Witness witness 	= txnrow.getWitness();
		
		//get the tip..
		TxPoWTreeNode tip 		= MinimaDB.getDB().getTxPoWTree().getTip();
		MiniNumber currentblock = tip.getBlockNumber();
		
		//Which node are we going to use..
		MiniNumber minblock = currentblock.sub(GlobalParams.MINIMA_MMR_PROOF_HISTORY);
		
		//Get all the inputs
		ArrayList<Coin> coins = trans.getAllInputs();
		
		//Add the inputs..
		for(Coin input : coins) {
			//How deep
			if(input.getBlockCreated().isMore(minblock)) {
				minblock = input.getBlockCreated();
			}
		}
		
		//Now get that Tree node
		TxPoWTreeNode mmrnode = tip.getPastNode(minblock);
		
		//Cycle through thte inputs..
		for(Coin input : coins) {
			
			//Get the proof..
			MMRProof proof = mmrnode.getMMR().getProofToPeak(input.getMMREntryNumber());
			
			//Create the CoinProof..
			CoinProof cp = new CoinProof(input, proof);
			
			//Add it to the witness data
			witness.addCoinProof(cp);
		}
		
		//Output the current trans..
		ret.put("response", db.getTransactionRow(id).toJSON());
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new txnmmr();
	}

}
