package org.minima.system.brains;

import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.objects.Coin;
import org.minima.objects.base.MiniData;

public class TxPoWSearcher {

	
	public static ArrayList<Coin> getRelevantUnspentCoins(TxPoWTreeNode zStartNode) {
		return getRelevantUnspentCoins(zStartNode, "");
	}
	
	public static ArrayList<Coin> getRelevantUnspentCoins(TxPoWTreeNode zStartNode, String zTokenID ) {
		
		//The list of Coins
		ArrayList<Coin> coinentry = new ArrayList<>();
		
		//Get the tip..
		TxPoWTreeNode tip = zStartNode;
		
		//A list of spent CoinID..
		ArrayList<String> spentcoins = new ArrayList<>();
		
		//Now cycle through and get all your coins..
		while(tip != null) {
			
			//Get the Relevant coins..
			ArrayList<Coin> coins = tip.getRelevantCoins();
			
			//Get the details..
			for(Coin coin : coins) {
				
				//Are we searching for a specific token..
				if(!zTokenID.equals("") && !coin.getTokenID().to0xString().equals(zTokenID)) {
					continue;
				}
				
				//Get the CoinID
				String coinid = coin.getCoinID().to0xString();
				
				//is it spent..
				boolean spent = coin.getSpent();
				
				//Add it to our list of spent coins..
				if(spent) {
					spentcoins.add(coinid);
				}else {
					//Check if this has been spent in a previous block..
					if(!spentcoins.contains(coinid)) {
						
						//OK - fresh unspent coin
						coinentry.add(coin);
					}
				}
			}
			
			//And move back up the tree
			tip = tip.getParent();
		}
		
		return coinentry;
	}
	
	public static ArrayList<Coin> searchCoins(	boolean zRelevant, 
												boolean zCheckCoinID, MiniData zCoinID,
												boolean zCheckAddress, MiniData zAddress,
												boolean zCheckTokenID, MiniData zTokenID) {
		
		//The list of Coins
		ArrayList<Coin> coinentry = new ArrayList<>();
		
		//Get the tip..
		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
		
		//A list of spent CoinID..
		ArrayList<String> spentcoins = new ArrayList<>();
		
		//Now cycle through and get all your coins..
		while(tip != null) {
			
			//Get the Relevant coins..
			ArrayList<Coin> coins = null;
			if(zRelevant) {
				coins = tip.getRelevantCoins();
			}else {
				coins = tip.getAllCoins();
			}
			
			//Get the details..
			for(Coin coin : coins) {
				
				//Are we searching for a specific token..
				if(zCheckTokenID && !coin.getTokenID().isEqual(zTokenID)) {
					continue;
				}
				
				if(zCheckCoinID && !coin.getCoinID().isEqual(zCoinID)) {
					continue;
				}
				
				if(zCheckAddress && !coin.getAddress().isEqual(zAddress)) {
					continue;
				}
				
				//Get the CoinID
				String coinid = coin.getCoinID().to0xString();
				
				//is it spent..
				boolean spent = coin.getSpent();
				
				//Add it to our list of spent coins..
				if(spent) {
					spentcoins.add(coinid);
				}else {
					//Check if this has been spent in a previous block..
					if(!spentcoins.contains(coinid)) {
						
						//OK - fresh unspent coin
						coinentry.add(coin);
					}
				}
			}
			
			//And move back up the tree
			tip = tip.getParent();
		}
		
		return coinentry;
	}
		
}
