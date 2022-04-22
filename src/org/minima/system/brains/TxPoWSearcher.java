package org.minima.system.brains;

import java.util.ArrayList;
import java.util.HashSet;

import org.minima.database.MinimaDB;
import org.minima.database.txpowdb.TxPoWDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Coin;
import org.minima.objects.Token;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

public class TxPoWSearcher {

	
	public static ArrayList<Coin> getAllRelevantUnspentCoins(TxPoWTreeNode zStartNode) {
		
		//Special search..
		return searchCoins(zStartNode, true, 
							false, MiniData.ZERO_TXPOWID,
							false, MiniNumber.ZERO,
							false, MiniData.ZERO_TXPOWID,
							false, MiniData.ZERO_TXPOWID,false);
		
	}
	
	public static ArrayList<Coin> getRelevantUnspentCoins(TxPoWTreeNode zStartNode, String zTokenID, boolean zSimpleOnly ) {
		
		//Special search..
		return searchCoins(zStartNode, true, 
							false, MiniData.ZERO_TXPOWID, 
							false, MiniNumber.ZERO,
							false, MiniData.ZERO_TXPOWID,
							true, new MiniData(zTokenID),
							zSimpleOnly);
	
	}
	
	public static Coin searchCoin(	MiniData zCoinID ){
		
		ArrayList<Coin> coins = searchCoins(MinimaDB.getDB().getTxPoWTree().getTip(), false, 
											true, zCoinID, 
											false, MiniNumber.ZERO,
											false, MiniData.ZERO_TXPOWID,
											false, MiniData.ZERO_TXPOWID,false);
		
		//Did we find it
		if(coins.size()>0) {
			return coins.get(0);
		}else {
			return null;
		}
	}
	
	public static Coin getFloatingCoin(TxPoWTreeNode zStartNode, MiniNumber zAmount, MiniData zAddress, MiniData zTokenID ) {
		
		//Special search..
		ArrayList<Coin> coins =  searchCoins(zStartNode, false, 
												false, MiniData.ZERO_TXPOWID, 
												true, zAmount,
												true, zAddress,
												true, zTokenID,
												false);
		
		//Did we find it
		if(coins.size()>0) {
			return coins.get(0);
		}else {
			return null;
		}
	}
	
	public static ArrayList<Coin> searchCoins(	TxPoWTreeNode zStartNode, boolean zRelevant, 
												boolean zCheckCoinID, MiniData zCoinID,
												boolean zCheckAmount, MiniNumber zAmount,
												boolean zCheckAddress, MiniData zAddress,
												boolean zCheckTokenID, MiniData zTokenID,
												boolean zSimpleOnly) {
		
		//The list of Coins
		ArrayList<Coin> coinentry = new ArrayList<>();
		
		//Start node position
		TxPoWTreeNode tip = zStartNode;
		
		//A list of spent CoinID..
		HashSet<String> spentcoins = new HashSet<>();
		
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
				
				if(zCheckAmount && !coin.getAmount().isEqual(zAmount)) {
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
						coinentry.add(coin.deepCopy());
						
						//And no more from now..
						spentcoins.add(coinid);
					}
				}
			}
			
			//And move back up the tree
			tip = tip.getParent();
		}
		
		//Are we only showing simple Coins..
		ArrayList<Coin> finalcoins = coinentry;
		if(zSimpleOnly) {
			//Fresh List
			finalcoins = new ArrayList<>();
			
			//Get the wallet..
			Wallet wallet = MinimaDB.getDB().getWallet();
			
			//Now cycle through the coins
			for(Coin cc : coinentry) {
				
				//Is it a simple address
				if(wallet.isAddressSimple(cc.getAddress().to0xString())) {
					finalcoins.add(cc);
				}
			}
		}
		
//		if(zSimpleOnly) {
//			//Fresh List
//			finalcoins = new ArrayList<>();
//			
//			//Get the wallet..
//			Wallet wallet = MinimaDB.getDB().getWallet();
//			
//			//Get all the keys
//			ArrayList<KeyRow> keys = wallet.getAllRelevant(false);
//			
//			//Now cycle through the coins
//			for(Coin cc : coinentry) {
//				for(KeyRow kr : keys) {
//					//Is it a simple key
//					if(!kr.getPublicKey().equals("")) {
//						if(cc.getAddress().isEqual(new MiniData(kr.getAddress()))) {
//							finalcoins.add(cc);
//						}
//					}
//				}
//			}
//		}
		
		return finalcoins;
	}	
	
	public static TxPoWTreeNode getTreeNodeForCoin(MiniData zCoinID) {
		
		//Start node position
		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
		
		//Now cycle through and get all your coins..
		while(tip != null) {

			//Get ALL the coins..
			ArrayList<Coin> coins = tip.getAllCoins();
			
			//Get the details..
			for(Coin coin : coins) {
				
				//Is this the one..
				if(coin.getCoinID().equals(zCoinID)) {
					return tip;
				}
			}
			
			//And move back up the tree
			tip = tip.getParent();
		}
		
		return null;
	}
	
	public static TxPoW getTxPoWBlock(MiniNumber zBlockNumber) {
		
		//Start node position
		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
		
		//Now cycle through and get all your coins..
		while(tip != null) {

			//Is this the block
			if(tip.getBlockNumber().isEqual(zBlockNumber)) {
				return tip.getTxPoW();
			}
			
			//And move back up the tree
			tip = tip.getParent();
		}
		
		return null;
	}

	public static ArrayList<TxPoW> searchTxPoWviaAddress(MiniData zAddress) {
		
		ArrayList<TxPoW> ret = new ArrayList<>();
		
		//Start node position
		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
		
		//The TxPoWDB
		TxPoWDB txpdb = MinimaDB.getDB().getTxPoWDB();
		
		//Now cycle through and get all your coins..
		while(tip != null) {

			//Get ALL the coins..
			ArrayList<Coin> coins = tip.getAllCoins();
			
			//Get the details..
			for(Coin coin : coins) {
				
				//Is this the one..
				if(coin.getAddress().equals(zAddress)) {
					
					//This block has this address somewhere.. find it..
					TxPoW txblock = tip.getTxPoW();
					
					//Search ..
					if(checkTxPoWForAddress(txblock, zAddress)) {
						ret.add(txblock);
					}
					
					//Check all the transactions..
					ArrayList<MiniData> txns = txblock.getBlockTransactions();
					for(MiniData txn : txns) {
						
						//Get this TxPoW..
						TxPoW txp = txpdb.getTxPoW(txn.to0xString());
						if(txp != null) {
							if(checkTxPoWForAddress(txp, zAddress)) {
								ret.add(txp);
							}
						}
					}
				}
			}
			
			//And move back up the tree
			tip = tip.getParent();
		}
		
		return ret;
	}
	
	public static boolean checkTxPoWForAddress(TxPoW zTxPoW, MiniData zAddress) {
		
		ArrayList<Coin> coins = zTxPoW.getTransaction().getAllInputs();
		for(Coin cc : coins) {
			if(cc.getAddress().isEqual(zAddress)) {
				return true;
			}
		}
		
		coins = zTxPoW.getBurnTransaction().getAllInputs();
		for(Coin cc : coins) {
			if(cc.getAddress().isEqual(zAddress)) {
				return true;
			}
		}
		
		coins = zTxPoW.getTransaction().getAllOutputs();
		for(Coin cc : coins) {
			if(cc.getAddress().isEqual(zAddress)) {
				return true;
			}
		}
		
		coins = zTxPoW.getBurnTransaction().getAllOutputs();
		for(Coin cc : coins) {
			if(cc.getAddress().isEqual(zAddress)) {
				return true;
			}
		}
		
		return false;
	}
	
	public static ArrayList<Token> getAllTokens() {

		//The list of Tokens - not including Minima
		ArrayList<Token> tokens = new ArrayList<>();
		
		//Start node position
		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
		
		//A list of added tokens
		HashSet<String> added = new HashSet<>();
		
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
	
	public static Token getToken(MiniData zTokenID) {
		
		//Start node position
		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
		
		//Now cycle through and get all your coins..
		while(tip != null) {

			//Get ALL the coins..
			ArrayList<Coin> coins = tip.getAllCoins();
			
			//Get the details..
			for(Coin coin : coins) {
				
				//Is this the one..
				if(coin.getTokenID().isEqual(zTokenID)) {
					return coin.getToken();
				}
			}
			
			//And move back up the tree
			tip = tip.getParent();
		}
		
		return null;
	}
}

		