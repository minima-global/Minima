package org.minima.system.brains;

import java.util.ArrayList;
import java.util.HashSet;

import org.minima.database.MinimaDB;
import org.minima.database.txpowdb.TxPoWDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Coin;
import org.minima.objects.StateVariable;
import org.minima.objects.Token;
import org.minima.objects.TxBlock;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.params.GeneralParams;

public class TxPoWSearcher {

	/**
	 * A temporary list of imported tokens.. 
	 */
	private static ArrayList<Token> mImportedTokens = new ArrayList<>();
	
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
		return searchCoin(zCoinID, GeneralParams.IS_MEGAMMR);
	}
	
	public static Coin searchCoin(	MiniData zCoinID , boolean zMegaMMR){
		
		ArrayList<Coin> coins = searchCoins(MinimaDB.getDB().getTxPoWTree().getTip(), false, 
											true, zCoinID, 
											false, MiniNumber.ZERO,
											false, MiniData.ZERO_TXPOWID,
											false, MiniData.ZERO_TXPOWID,
											false, "", false,
											false, Integer.MAX_VALUE, zMegaMMR);
		
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
		
		//Search for any depth
		return searchCoins(zStartNode, zRelevant, zCheckCoinID, zCoinID, zCheckAmount, 
				zAmount, zCheckAddress, zAddress, zCheckTokenID, zTokenID, zSimpleOnly, Integer.MAX_VALUE);
	}
	
	public static ArrayList<Coin> searchCoins(	TxPoWTreeNode zStartNode, boolean zRelevant, 
			boolean zCheckCoinID, MiniData zCoinID,
			boolean zCheckAmount, MiniNumber zAmount,
			boolean zCheckAddress, MiniData zAddress,
			boolean zCheckTokenID, MiniData zTokenID,
			boolean zSimpleOnly, int zDepth) {
		
		return searchCoins(zStartNode, zRelevant, zCheckCoinID, zCoinID, zCheckAmount, 
				zAmount, zCheckAddress, zAddress, zCheckTokenID, zTokenID, 
				false, "", false,
				zSimpleOnly, zDepth,false);
	}
	
	public static ArrayList<Coin> searchCoins(	TxPoWTreeNode zStartNode, boolean zRelevant, 
												boolean zCheckCoinID, MiniData zCoinID,
												boolean zCheckAmount, MiniNumber zAmount,
												boolean zCheckAddress, MiniData zAddress,
												boolean zCheckTokenID, MiniData zTokenID,
												boolean zCheckState, String zState, boolean zWildCardState,
												boolean zSimpleOnly, int zDepth,boolean zMEGAMMR) {
		
		//The list of Coins
		ArrayList<Coin> coinentry = new ArrayList<>();
		
		//Start node position
		TxPoWTreeNode tip = zStartNode;
		
		//A list of spent CoinID..
		HashSet<String> spentcoins = new HashSet<>();
		
		//Now cycle through and get all your coins..
		int depth = 0;
		
		//Are we MEGAMMR
		boolean MEGACHECK = false; 
		
		//Cycle through
		while(tip!=null || MEGACHECK) {
			
			//Are we deep enough
			if(depth++>zDepth) {
				break;
			}
			
			ArrayList<Coin> coins = null;
			if(!MEGACHECK) {
				//Get the Relevant coins..
				if(zRelevant) {
					coins = tip.getRelevantCoins();
				}else {
					coins = tip.getAllCoins();
				}
			}else {
				//Need to LOCK DB
				MinimaDB.getDB().readLock(true);
				
				//Get the MEGAMMR COINS..
				coins = new ArrayList<Coin>(MinimaDB.getDB().getMegaMMR().getAllCoins().values());
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
				
				if(zCheckState && !coin.checkForStateVariable(zState,zWildCardState)) {
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
						
						//Make a copy..
						Coin copycoin = coin.deepCopy();
						
						//OK - fresh unspent coin
						coinentry.add(copycoin);
						
						//And no more from now..
						spentcoins.add(coinid);
					}
				}
			}
			
			if(!MEGACHECK) {
				//And move back up the tree
				tip = tip.getParent();
				
				//Are we at the end..
				if(tip == null && zMEGAMMR) {
					MEGACHECK = true;
				}
			}else {
				//Need to LOCK DB
				MinimaDB.getDB().readLock(false);
				
				//we just did a MEGAMMR check.. that's it..
				break;
			}
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

	public static TxPoW searchChainForTxPoW(MiniData zTxPoWID) {
		
		//Start node position
		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
		
		//Now cycle through and get all your coins..
		while(tip != null) {

			//The Block
			TxPoW txblock = tip.getTxPoW();
			
			//Is this block the txn
			if(txblock.getTxPoWIDData().isEqual(zTxPoWID)) {
				return txblock;
			}
			
			//Check all the transactions..
			ArrayList<MiniData> txns = txblock.getBlockTransactions();
			for(MiniData txn : txns) {
				
				//Is this the one..
				if(txn.isEqual(zTxPoWID)) {
					return txblock;
				}
			}
			
			
			//And move back up the tree
			tip = tip.getParent();
		}
		
		return null;
	}

	public static TxPoWTreeNode searchChainForTxPoWBlock(MiniData zTxPoWID) {
		
		//Start node position
		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
		
		//Now cycle through and get all your coins..
		while(tip != null) {

			//The Block
			TxPoW txblock = tip.getTxPoW();
			
			//Is this block the txn
			if(txblock.getTxPoWIDData().isEqual(zTxPoWID)) {
				return tip;
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
	
	public static boolean checkTxPoWRelevant(TxPoW zTxPoW, Wallet zWallet) {
		
		ArrayList<Coin> coins = zTxPoW.getTransaction().getAllInputs();
		for(Coin cc : coins) {
			String address = cc.getAddress().to0xString();
			if(zWallet.isAddressRelevant(address)) {
				return true;
			}
			
			//Check the state vars
			ArrayList<StateVariable> state = cc.getState();
			for(StateVariable sv : state) {
				
				if(sv.getType().isEqual(StateVariable.STATETYPE_HEX)) {
					String svstr = sv.toString();
					
					//Custom scripts have no public key..
					if(zWallet.isAddressRelevant(svstr) || zWallet.isKeyRelevant(svstr)) {
						return true;
					}
				}
			}
		}
		
		coins = zTxPoW.getBurnTransaction().getAllInputs();
		for(Coin cc : coins) {
			String address = cc.getAddress().to0xString();
			if(zWallet.isAddressRelevant(address)) {
				return true;
			}
			
			//Check the state vars
			ArrayList<StateVariable> state = cc.getState();
			for(StateVariable sv : state) {
				
				if(sv.getType().isEqual(StateVariable.STATETYPE_HEX)) {
					String svstr = sv.toString();
					
					//Custom scripts have no public key..
					if(zWallet.isAddressRelevant(svstr) || zWallet.isKeyRelevant(svstr)) {
						return true;
					}
				}
			}
		}
		
		coins = zTxPoW.getTransaction().getAllOutputs();
		for(Coin cc : coins) {
			String address = cc.getAddress().to0xString();
			if(zWallet.isAddressRelevant(address)) {
				return true;
			}
		}
		
		coins = zTxPoW.getBurnTransaction().getAllOutputs();
		for(Coin cc : coins) {
			String address = cc.getAddress().to0xString();
			if(zWallet.isAddressRelevant(address)) {
				return true;
			}
		}
		
		//And now check the state vars
		ArrayList<StateVariable> state = zTxPoW.getTransaction().getCompleteState();
		for(StateVariable sv : state) {
			
			if(sv.getType().isEqual(StateVariable.STATETYPE_HEX)) {
				String svstr = sv.toString();
				
				//Custom scripts have no public key..
				if(zWallet.isAddressRelevant(svstr) || zWallet.isKeyRelevant(svstr)) {
					return true;
				}
			}
		}
		
		state = zTxPoW.getBurnTransaction().getCompleteState();
		for(StateVariable sv : state) {
			
			if(sv.getType().isEqual(StateVariable.STATETYPE_HEX)) {
				String svstr = sv.toString();
				
				//Custom scripts have no public key..
				if(zWallet.isAddressRelevant(svstr) || zWallet.isKeyRelevant(svstr)) {
					return true;
				}
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
		
		//Search the imported tokens.. 
		for(Token tok : mImportedTokens) {
			
			//Get the TokenID
			String tokenid = tok.getTokenID().to0xString();
			
			//Have we added it already
			if(!added.contains(tokenid)) {
				
				//Add to our list
				added.add(tokenid);
				
				//And add to our main array
				tokens.add(tok);
			}
		}
		
		return tokens;
	}
	
	
	public static void importToken(Token zToken) {
		mImportedTokens.add(zToken);
	}
	
	public static Token getToken(MiniData zTokenID) {
		
		//Search the imported tokens first as faster.. 
		for(Token tok : mImportedTokens) {
			
			//Check the tokenid
			if(tok.getTokenID().isEqual(zTokenID)) {
				return tok;
			}
		}
		
		//Start node position
		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
		
		//Are we MEGAMMR
		boolean MEGACHECK = false; 
		
		//Cycle through
		while(tip!=null || MEGACHECK) {
		

			//Get ALL the coins..
			ArrayList<Coin> coins = null;
			if(!MEGACHECK) {
				coins = tip.getAllCoins();
			}else {
				//Need to LOCK DB
				MinimaDB.getDB().readLock(true);
				
				//Get the MEGAMMR COINS..
				coins = new ArrayList<Coin>(MinimaDB.getDB().getMegaMMR().getAllCoins().values());
			}
			
			//Get the details..
			for(Coin coin : coins) {
				
				//Is this the one..
				if(coin.getTokenID().isEqual(zTokenID)) {
					return coin.getToken();
				}
			}
			
			if(!MEGACHECK) {
				//And move back up the tree
				tip = tip.getParent();
				
				//Are we at the end..
				if(tip == null && GeneralParams.IS_MEGAMMR) {
					MEGACHECK = true;
				}
			}else {
				
				//Need to LOCK DB
				MinimaDB.getDB().readLock(false);
				
				//we just did a MEGAMMR check.. that's it..
				break;
			}
		}
		
		return null;
	}
}

		