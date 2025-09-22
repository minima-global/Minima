package org.minima.system.commands.txn;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.database.wallet.ScriptRow;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Coin;
import org.minima.objects.ScriptProof;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.mmr.MMRProof;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.json.JSONObject;

public class txnscript extends Command {

	public txnscript() {
		super("txnscript","[id:] (auto:false) (scripts:{}) - Add scripts to a txn");
	}
	
	@Override
	public String getFullHelp() {
		return "\ntxnscript\n"
				+ "\n"
				+ "Add scripts to a transaction.\n"
				+ "\n"
				+ "id:\n"
				+ "    The id of the transaction.\n"
				+ "\n"
				+ "auto:\n"
				+ "    Automatically add scripts you know. Useful for multi user transactions.\n"
				+ "\n"
				+ "scripts:\n"
				+ "    JSON holds the script and the proof in the format {script:proof}\n"
				+ "    If it is a single script, and not one created with mmrcreate, leave the proof blank.\n"
				+ "    If it is an mmrcreate script, include the proof.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "txnscript id:txnmast scripts:{\"RETURN TRUE\":\"\"}\n"
				+ "\n"
				+ "txnscript id:txnmast scripts:{\"RETURN TRUE\":\"0x000..\"}\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"id","scripts","auto"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();

		TxnDB db = MinimaDB.getDB().getCustomTxnDB();
		
		//The transaction
		String id 			= getParam("id");
		JSONObject scripts  = getJSONObjectParam("scripts", new JSONObject());
		boolean auto 		= getBooleanParam("auto", false);
		
		//Get the Transaction
		TxnRow txnrow 	= db.getTransactionRow(getParam("id"));
		if(txnrow == null) {
			throw new CommandException("Transaction not found : "+id);
		}
		Witness witness 	= txnrow.getWitness();
		
		ArrayList<String> addedscripts = new ArrayList<String>(); 
		
		//Any extra scripts
		if(auto) {
			
			//Need the transaction
			Transaction trans 	= txnrow.getTransaction();
			
			//Get the main Wallet
			Wallet walletdb = MinimaDB.getDB().getWallet();
			
			//Get all the inputs
			ArrayList<Coin> inputs = trans.getAllInputs();
			for(Coin input : inputs) {
				
				//Is the script missing
				if(witness.getScript(input.getAddress()) == null){
					String scraddress 	= input.getAddress().to0xString();
					ScriptRow srow 		= walletdb.getScriptFromAddress(scraddress);
					if(srow != null) {
						ScriptProof pscr = new ScriptProof(srow.getScript());
						witness.addScript(pscr);
						
						addedscripts.add(pscr.getScript().toString());
					}
				}
			}
			
		}else {
			for(Object key : scripts.keySet()) {
				
				//Get the script
				String exscript = (String)key;
				
				//The Key is a String
				String proof 		=  (String) scripts.get(key);
				ScriptProof scprf 	= null;
				if(proof.equals("")) {
					//Create a ScriptProof..
					scprf = new ScriptProof(exscript);
					
				}else {
					MiniData proofdata 	= new MiniData(proof); 
					
					//Make it into an MMRProof..
					MMRProof scproof = MMRProof.convertMiniDataVersion(proofdata);
					
					//Create a ScriptProof..
					scprf = new ScriptProof(exscript, scproof);
				}
				
				//Add to the Witness..
				witness.addScript(scprf);
				
				addedscripts.add(scprf.getScript().toString());
			}
		}
		
		
		//Output the current trans..
		ret.put("response", db.getTransactionRow(id).toJSON());
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new txnscript();
	}

}
