package org.minima.system.commands.base;

import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMRProof;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.objects.Coin;
import org.minima.objects.CoinProof;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class coinimport extends Command {

	public coinimport() {
		super("coinimport","[data:] - Import a coin, and keep tracking it");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		String data = getParam("data");
		
		//Convert to a coin proof..
		CoinProof cp = CoinProof.convertMiniDataVersion(new MiniData(data));
		
		//Check is UNSPENT..
		if(cp.getCoin().getSpent()) {
			throw new CommandException("Coin is spent. Can only import UNSPENT coins.");
		}
		
		//Get the tip
		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
				
		//Now check that proof..
		boolean valid = tip.getMMR().checkProofTimeValid(cp.getCoin().getMMREntryNumber(), cp.getMMRData(), cp.getMMRProof());
		if(!valid) {
			throw new CommandException("Invalid MMR Proof");
		}
		
		//Ok.. now we have to add this to OUR TreeNode MMR..
		
		
		ret.put("response", cp.toJSON());
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new coinimport();
	}

}
