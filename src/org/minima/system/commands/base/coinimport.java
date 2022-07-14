package org.minima.system.commands.base;

import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMRData;
import org.minima.database.mmr.MMRProof;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.objects.Coin;
import org.minima.objects.CoinProof;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.json.JSONObject;

public class coinimport extends Command {

	public coinimport() {
		super("coinimport","[data:] (track:false) - Import a coin, and keep tracking it");
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
			throw new CommandException("Coin is spent. Can only import UNSPENT coins.");
		}
		
		//Get the tip
		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
		
		//Do we already have it..
		MMRProof checkproof = tip.getMMR().getProofToPeak(newcoin.getMMREntryNumber());
		CoinProof currentproof 	= new CoinProof(newcoin, checkproof);
		boolean currentvalid 	= tip.getMMR().checkProofTimeValid(newcoin.getMMREntryNumber(), currentproof.getMMRData(), currentproof.getMMRProof());
		if(currentvalid) {
			
			//Get the Tree Node..
			TxPoWTreeNode node = TxPoWSearcher.getTreeNodeForCoin(newcoin.getCoinID());
			
			//Is it relevant..
			if(node.isRelevantEntry(newcoin.getMMREntryNumber())) {
				throw new CommandException("Attempting to add relevant coin we already have");
			}
			
			//Add to relevant coins..
			node.getRelevantCoinsEntries().add(newcoin.getMMREntryNumber());
			node.calculateRelevantCoins();
		
			//Added
			ret.put("response", newcoinproof.toJSON());
			return ret;
		}
		
		//Now check that newcoinproof is valid..
		boolean valid = tip.getMMR().checkProofTimeValid(newcoin.getMMREntryNumber(), newcoinproof.getMMRData(), newcoinproof.getMMRProof());
		if(!valid) {
			throw new CommandException("Invalid MMR Proof");
		}
		
		//When is this CoinProof..
		MiniNumber coinblock = newcoinproof.getMMRProof().getBlockTime();
		
		//Ok.. now we have to add this to OUR TreeNode MMR..
		TxPoWTreeNode treenode = tip.getPastNode(coinblock);
		if(treenode==null) {
			throw new CommandException("TreeNode at Blocktime not found (proof too old): "+coinblock);
		}
		
		//Checker..
		MMRData oldroot = treenode.getMMR().getRoot();
		
		//And create a new MMRData with the correct amount
		MMRData mmrdata = newcoinproof.getMMRData();

		//About to change the MMR..
		treenode.getMMR().setFinalized(false);
		
		//And add to the MMR
		treenode.getMMR().updateEntry(newcoin.getMMREntryNumber(), newcoinproof.getMMRProof(), newcoinproof.getMMRData());	

		//Re-finalise
		treenode.getMMR().finalizeSet();
				
		//Add to the total List of coins fro this block
		treenode.getAllCoins().add(newcoin);
		
		//And set to relevant.. track it..
		if(getBooleanParam("track", true)) {
			treenode.getRelevantCoins().add(newcoin);
			treenode.getRelevantCoinsEntries().add(newcoin.getMMREntryNumber());
		}
		
		//New root..
		MMRData newroot = treenode.getMMR().getRoot();
		if(!newroot.isEqual(oldroot)) {
			throw new CommandException("SERIOUS ERROR : MMR root different after adding coin.. "+newcoinproof.toJSON().toString());
		}
		
		ret.put("response", newcoinproof.toJSON());
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new coinimport();
	}

}
