package org.minima.system.brains;

import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.objects.Coin;
import org.minima.objects.Token;
import org.minima.objects.base.MiniData;

public class TxPoWSearcher {

	
	public static ArrayList<Coin> getRelevantUnspentCoins(TxPoWTreeNode zStartNode) {
		
		//Special search..
		return searchCoins(zStartNode, true, 
							false, MiniData.ZERO_TXPOWID, 
							false, MiniData.ZERO_TXPOWID,
							false, MiniData.ZERO_TXPOWID);
		
	}
	
	public static ArrayList<Coin> getRelevantUnspentCoins(TxPoWTreeNode zStartNode, String zTokenID ) {
		
		//Special search..
		return searchCoins(zStartNode, true, 
							false, MiniData.ZERO_TXPOWID, 
							false, MiniData.ZERO_TXPOWID,
							true, new MiniData(zTokenID));
	
	}
	
	public static ArrayList<Coin> searchCoins(	TxPoWTreeNode zStartNode, boolean zRelevant, 
												boolean zCheckCoinID, MiniData zCoinID,
												boolean zCheckAddress, MiniData zAddress,
												boolean zCheckTokenID, MiniData zTokenID) {
		
		//The list of Coins
		ArrayList<Coin> coinentry = new ArrayList<>();
		
		//Start node position
		TxPoWTreeNode tip = zStartNode;
		
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
						
						//And no more from now..
						spentcoins.add(coinid);
					}
				}
			}
			
			//And move back up the tree
			tip = tip.getParent();
		}
		
		return coinentry;
	}	
	
	public static ArrayList<Token> getAllTokens() {

		//The list of Tokens - not including Minima
		ArrayList<Token> tokens = new ArrayList<>();
		
		//Start node position
		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
		
		//A list of added tokens
		ArrayList<String> added = new ArrayList<>();
		
		//Now cycle through and get all your coins..
		while(tip != null) {

			//Get ALL the coins..
			ArrayList<Coin> coins = tip.getAllCoins();
			
			//Get the details..
			for(Coin coin : coins) {
				
				//Get the TokenID
				String tokenid = coin.getTokenID().to0xString();
				
				//Add it to our list of spent coins..
				if(!tokenid.equals("0x00")) {
					
					//Have we added it already
					if(!added.contains(tokenid)) {
						
						//Add to our list
						added.add(tokenid);
						
						//And add to our main array
						tokens.add(coin.getToken());
					}
				}
			}
			
			//And move back up the tree
			tip = tip.getParent();
		}
		
		return tokens;
	}
}

		