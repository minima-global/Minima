package org.minima.system.commands.base;

import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.objects.Coin;
import org.minima.objects.base.MiniData;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.json.JSONObject;

public class cointrack extends Command {

	public cointrack() {
		super("cointrack","[enable:true|false] [coinid:] - Track or untrack a coin");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		String coinid 		= getParam("coinid");
		MiniData coindata 	= new MiniData(coinid);
		boolean track 		= getBooleanParam("enable");
		
		//Get the Tree Node..
		TxPoWTreeNode node = TxPoWSearcher.getTreeNodeForCoin(new MiniData(coinid));
		if(node == null) {
			throw new CommandException("Coin not found coinid : "+coinid);
		}
		
		//Get the coin..
		Coin coin = TxPoWSearcher.searchCoin(coindata);
		
		//Are we enabling or disabling..
		if(track) {
			//Check if we are already tracking..
			if(node.isRelevantEntry(coin.getMMREntryNumber())) {
			
				//Already tracked..
				ret.put("response", "Coin already tracked");
			
			}else {
				
				//Add to relevant coins..
				node.getRelevantCoinsEntries().add(coin.getMMREntryNumber());
				node.calculateRelevantCoins();
				
				ret.put("response", "Coin added to track list");
			}
		}else {
			//Stop tracking
			node.removeRelevantCoin(coin.getMMREntryNumber());
			node.calculateRelevantCoins();
			
			ret.put("response", "Coin removed from track list");
		}
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new cointrack();
	}

}
