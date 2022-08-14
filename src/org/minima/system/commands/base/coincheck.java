package org.minima.system.commands.base;

import org.minima.database.MinimaDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.objects.Coin;
import org.minima.objects.CoinProof;
import org.minima.objects.base.MiniData;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.json.JSONObject;

public class coincheck extends Command {

	public coincheck() {
		super("coincheck","[data:] - Check a coin exists");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		String data = getParam("data");
		
		//Convert to a coin proof..
		CoinProof newcoinproof 	= CoinProof.convertMiniDataVersion(new MiniData(data));
		
		//The coin and Proof..
		Coin newcoin 			= newcoinproof.getCoin();
		
		//Check is UNSPENT..
		if(newcoin.getSpent()) {
			throw new CommandException("Coin is spent. Can only check UNSPENT coins.");
		}
		
		//Get the tip
		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
		
		//Check the MMR
		boolean validmmr = tip.getMMR().checkProofTimeValid(newcoinproof.getCoin().getMMREntryNumber(), newcoinproof.getMMRData(), newcoinproof.getMMRProof());
		
		//Add to response
		JSONObject resp = new JSONObject();
		resp.put("proofblock", newcoinproof.getMMRProof().getBlockTime());
		resp.put("coin", newcoin.toJSON());
		resp.put("valid", validmmr);
		
		ret.put("response", resp);
				
		return ret;
	}

	@Override
	public Command getFunction() {
		return new coincheck();
	}

}
