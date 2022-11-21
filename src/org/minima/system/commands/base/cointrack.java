package org.minima.system.commands.base;

import java.util.ArrayList;
import java.util.Arrays;

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
	public String getFullHelp() {
		return "\ncointrack\n"
				+ "\n"
				+ "Track or untrack a coin.\n"
				+ "\n"
				+ "Track a coin to keep its MMR proof up-to-date and know when it becomes spent. Stop tracking to remove it from your relevant coins list.\n"
				+ "\n"
				+ "enable:\n"
				+ "    true or false, true will add the coin to your relevant coins, false will remove it from your relevant coins.\n"
				+ "\n"
				+ "coinid:\n"
				+ "    The id of a coin. Can be found using the 'coins' command.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "cointrack enable:true coinid:0xCD34..\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"file","password"}));
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
