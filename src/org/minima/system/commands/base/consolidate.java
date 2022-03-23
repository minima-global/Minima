package org.minima.system.commands.base;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;

import org.minima.database.MinimaDB;
import org.minima.database.txpowdb.TxPoWDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.txpowtree.TxPowTree;
import org.minima.objects.Coin;
import org.minima.objects.Transaction;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.commands.Command;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;

public class consolidate extends Command {
	public consolidate() {
		super("consolidate","Consolidate multiple coins into single coins");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();

		//Get all the coins you own..
		TxPowTree txptree = MinimaDB.getDB().getTxPoWTree();
		
		//First scan the chain for coins you own that are simple..
		ArrayList<Coin> coins = TxPoWSearcher.getAllRelevantSimpleUnspentCoins(txptree.getTip());
		
		HashMap<String, MiniNumber> alltokencount = new HashMap<>();
		
		//Now work out what you have..
		for(Coin coin : coins) {
		
			//Get the token details..
			MiniData tokenid = coin.getTokenID();
			
			
			
			
			
			
			
			
		}
		
		

		return ret;
	}
	
	@Override
	public Command getFunction() {
		return new consolidate();
	}

}
