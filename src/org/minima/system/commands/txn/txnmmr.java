package org.minima.system.commands.txn;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMRProof;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.objects.ScriptProof;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.json.JSONObject;

public class txnmmr extends Command {

	public txnmmr() {
		super("txnmmr","[id:] [mmrproof:] - Add an MMR Proof");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"id","mmrproof"}));
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
		Witness witness = txnrow.getWitness();
		
//		//Any extra scripts
//		for(Object key : scripts.keySet()) {
//			
//			//Get the script
//			String exscript = (String)key;
//			
//			//The Key is a String
//			String proof 		=  (String) scripts.get(key);
//			ScriptProof scprf 	= null;
//			if(proof.equals("")) {
//				//Create a ScriptProof..
//				scprf = new ScriptProof(exscript);
//				
//			}else {
//				MiniData proofdata 	= new MiniData(proof); 
//				
//				//Make it into an MMRProof..
//				MMRProof scproof = MMRProof.convertMiniDataVersion(proofdata);
//				
//				//Create a ScriptProof..
//				scprf = new ScriptProof(exscript, scproof);
//			}
//			
//			//Add to the Witness..
//			witness.addScript(scprf);
//		}
		
		//Output the current trans..
		ret.put("response", db.getTransactionRow(id).toJSON());
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new txnmmr();
	}

}
