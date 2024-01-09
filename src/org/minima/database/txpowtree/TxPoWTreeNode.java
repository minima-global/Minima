package org.minima.database.txpowtree;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMR;
import org.minima.database.mmr.MMRData;
import org.minima.database.mmr.MMREntry;
import org.minima.database.mmr.MMREntryNumber;
import org.minima.database.mmr.MMRProof;
import org.minima.database.txpowdb.TxPoWDB;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Coin;
import org.minima.objects.CoinProof;
import org.minima.objects.StateVariable;
import org.minima.objects.TxBlock;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.Main;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;

public class TxPoWTreeNode implements Streamable {

	/**
	 * The SyncBlock that represents this Node
	 */
	TxBlock mTxBlock;
	
	/**
	 * Parent of this node
	 */
	TxPoWTreeNode mParent;
	
	/**
	 * Children
	 */
	ArrayList<TxPoWTreeNode> mChildren;
	
	/**
	 * The total weight of this node - GHOST
	 */
	BigDecimal mTotalWeight;
	
	/**
	 * The MMR - can be constructed from the TxBlock
	 */
	MMR mMMR;
	
	/**
	 * ALL The Coins - both spent and unspent
	 */
	ArrayList<Coin>	mCoins = new ArrayList<>();
	
	/**
	 * The Coins that are relevant to THIS USER
	 */
	ArrayList<MMREntryNumber> mRelevantMMRCoins = new ArrayList<>();
	
	/**
	 * Computed from previous data
	 */
	ArrayList<Coin> 	mComputedRelevantCoins = new ArrayList<>();
	
	/**
	 * Have we checked we have all the txns in this block
	 */
	boolean mHaveCheckedFull = false;
	
	private TxPoWTreeNode() {}
	
	public TxPoWTreeNode(TxBlock zTxBlock) {
		this(zTxBlock, true);
	}
	
	public TxPoWTreeNode(TxBlock zTxBlock, boolean zFindRelevant) {
		mTxBlock			= zTxBlock;
		mChildren 	 		= new ArrayList<>();
		mTotalWeight 		= BigDecimal.ZERO;
		mParent				= null;
		mHaveCheckedFull 	= false;
		
		//Construct the MMR..
		constructMMR(zFindRelevant);
	}
	
	//Used in tests..
	public TxPoWTreeNode(TxPoW zTestTxPoW) {
		mTxBlock		= new TxBlock(zTestTxPoW);
		mChildren 	 	= new ArrayList<>();
		mTotalWeight 	= BigDecimal.ZERO;
		mParent			= null;
		mMMR			= new MMR();
	}
	
	/**
	 * Convert the TxBlock 
	 */
	private void constructMMR(boolean zFindRelevant) {
		
		//What Block Time Are we..
		MiniNumber block = mTxBlock.getTxPoW().getBlockNumber();
				
		//Create a new MMR
		mMMR = new MMR();
		mMMR.setBlockTime(block);
		
		//Get the Wallet..
		Wallet wallet = null;
		if(zFindRelevant){
			wallet = MinimaDB.getDB().getWallet();
		}
		
		//Add all the peaks..
		ArrayList<MMREntry> peaks = mTxBlock.getPreviousPeaks();
		for(MMREntry peak : peaks) {
			mMMR.setEntry(peak.getRow(), peak.getEntryNumber(), peak.getMMRData());
		}
		
		//Calculate the Entry NUmber
		mMMR.calculateEntryNumberFromPeaks();
		
		//Has the balance changed
		boolean balancechange = false;
		
		//Now you have all the previous peaks.. update the spent coins..
		ArrayList<CoinProof> spentcoins = mTxBlock.getInputCoinProofs();
		for(CoinProof input : spentcoins) {
			
			//Which entry is this in the MMR
			MMREntryNumber entrynumber = input.getCoin().getMMREntryNumber();
			
			//A NEW MMRData of the spent coin
			Coin spentcoin = input.getCoin().deepCopy();
			spentcoin.setSpent(true);

			//Create the MMRData
			MMRData mmrdata = MMRData.CreateMMRDataLeafNode(spentcoin, MiniNumber.ZERO);
						
			//Update the MMR
			mMMR.updateEntry(entrynumber, input.getMMRProof(), mmrdata);
			
			//Add to the total List of coins for this block
			mCoins.add(spentcoin);
			
			//Is this Relevant to us..
			if(zFindRelevant) {
				if(checkRelevant(spentcoin, wallet)) {
					mRelevantMMRCoins.add(entrynumber);
					
					if(!GeneralParams.IS_MEGAMMR) {
						
						//Message..
						JSONObject coinjson = spentcoin.toJSON(true);
						MinimaLogger.log("NEW Spent Coin : "+coinjson);
						
						//Send a message
						JSONObject data = new JSONObject();
						data.put("relevant", true);
						data.put("txblockid", mTxBlock.getTxPoW().getTxPoWID());
						data.put("txblock", block.toString());
						data.put("spent", true);
						data.put("coin", coinjson);
						
						//And Post it..
						Main.getInstance().PostNotifyEvent(Main.MAIN_NEWCOIN, data);
					}					
				
					//There has been a balance change
					balancechange = true;
				}
				
				//Check the Coin Notify Details..
				if(!GeneralParams.IS_MEGAMMR) {
					String coinaddress = spentcoin.getAddress().to0xString();
					if(MinimaDB.getDB().checkCoinNotify(coinaddress)) {
						
						//Message..
						JSONObject coinjson = spentcoin.toJSON(true);
						//MinimaLogger.log("NOTIFY Spent Coin : "+coinjson);
						
						//Send a message
						JSONObject data = new JSONObject();
						data.put("address", coinaddress);
						data.put("txblockid", mTxBlock.getTxPoW().getTxPoWID());
						data.put("txblock", block.toString());
						data.put("spent", true);
						data.put("coin", coinjson);
						
						//And Post it..
						Main.getInstance().PostNotifyEvent(Main.MAIN_NOTIFYCOIN, data);
					}
				}
			}
		}
		
		//And ADD all the newly created coins
		ArrayList<Coin> outputs = mTxBlock.getOutputCoins();
		for(Coin output : outputs) {
			
			//Where are we in the MMR
			MMREntryNumber entrynumber = mMMR.getEntryNumber();
			
			//Create a new CoinMMR structure - unspent..
			Coin newcoin = output.deepCopy();
			newcoin.setMMREntryNumber(entrynumber);
			newcoin.setBlockCreated(block);
			newcoin.setSpent(false);
			
			//Create the MMRData
			MMRData mmrdata = MMRData.CreateMMRDataLeafNode(newcoin, output.getAmount());
						
			//And add to the MMR
			mMMR.addEntry(mmrdata);	
			
			//Add to the total List of coins for this block
			mCoins.add(newcoin);
			
			//Is this Relevant to us..
			if(zFindRelevant){
				if(checkRelevant(output, wallet)) {
					mRelevantMMRCoins.add(entrynumber);
					
					if(!GeneralParams.IS_MEGAMMR) {
						//Message..
						JSONObject coinjson = newcoin.toJSON(true);
						
						//Did we remove the state..
						if(!newcoin.storeState()) {
							//Get it..
							ArrayList<StateVariable> removedstate = mTxBlock.removedState(newcoin.getCoinID().to0xString());
							if(removedstate != null) {
								//Add it to the JSON
								coinjson.put("state", Coin.convertStateListToJSON(removedstate));
							}
						}
						
						MinimaLogger.log("NEW Unspent Coin : "+coinjson);
						
						//Send a message
						JSONObject data = new JSONObject();
						data.put("relevant", true);
						data.put("txblockid", mTxBlock.getTxPoW().getTxPoWID());
						data.put("txblock", block.toString());
						data.put("spent", false);
						data.put("coin", coinjson);
						
						//And Post it..
						Main.getInstance().PostNotifyEvent(Main.MAIN_NEWCOIN, data);
					}
					
					balancechange = true;
				}
				
				//Check the Coin Notify Details..
				if(!GeneralParams.IS_MEGAMMR) {
					String coinaddress = newcoin.getAddress().to0xString();
					if(MinimaDB.getDB().checkCoinNotify(coinaddress)) {
						
						//Message..
						JSONObject coinjson = newcoin.toJSON(true);
						
						//Did we remove the state..
						if(!newcoin.storeState()) {
							//Get it..
							ArrayList<StateVariable> removedstate = mTxBlock.removedState(newcoin.getCoinID().to0xString());
							if(removedstate != null) {
								//Add it to the JSON
								coinjson.put("state", Coin.convertStateListToJSON(removedstate));
							}
						}
						
						//MinimaLogger.log("NOTIFY Unspent Coin : "+coinjson);
						
						//Send a message
						JSONObject data = new JSONObject();
						data.put("address", coinaddress);
						data.put("txblockid", mTxBlock.getTxPoW().getTxPoWID());
						data.put("txblock", block.toString());
						data.put("spent", false);
						data.put("coin", coinjson);
						
						//And Post it..
						Main.getInstance().PostNotifyEvent(Main.MAIN_NOTIFYCOIN, data);
					}
				}
			}
		}
		
		//All done..!
		mMMR.finalizeSet();
		
		//Calculate the Relevant Coins..
		if(zFindRelevant) {
			calculateRelevantCoins();
		}
		
		//Has the balance changed
		if(!GeneralParams.IS_MEGAMMR && balancechange) {
			Main.getInstance().PostMessage(Main.MAIN_BALANCE);
		}
	}
	
	/**
	 * Check if this coin is relevant to this wallet and therefore should be kept
	 */
	private boolean checkRelevant(Coin zCoin, Wallet zWallet) {
		
		//Are we running in MEGA MMR MODE..
		if(GeneralParams.IS_MEGAMMR) {
			return true;
		}
		
		//Is the coin relevant to us..
		if(zWallet.isAddressRelevant(zCoin.getAddress().to0xString())) {
			return true;
		}
		
		//Are any of the state variables relevant to us..
		ArrayList<StateVariable> state = zCoin.getState();
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
	
	public void calculateRelevantCoins() {
		
		//Clear and start again
		mComputedRelevantCoins = new ArrayList<>();
		
		//Cycle through both lists..
		for(Coin coin : mCoins) {
			
			//MEGA MMR stores all coins
			if(GeneralParams.IS_MEGAMMR) {
				if(!coin.getSpent()) {
					mComputedRelevantCoins.add(coin);
				}
			}else {
				//Cycle the relevant
				for(MMREntryNumber relentry : mRelevantMMRCoins) {
					if(coin.getMMREntryNumber().isEqual(relentry)) {
						mComputedRelevantCoins.add(coin);
						break;
					}
				}
			}
		}
	}
	
	public TxBlock getTxBlock() {
		return mTxBlock;
	}
	
	public TxPoW getTxPoW() {
		return getTxBlock().getTxPoW();
	}
	
	public MiniNumber getBlockNumber() {
		return getTxPoW().getBlockNumber();
	}
	
	public MMR getMMR() {
		return mMMR;
	}
	
	public ArrayList<Coin> getAllCoins(){
		return mCoins;
	}
	
	public boolean isRelevantEntry(MMREntryNumber zMMREntryNumber) {
		for(MMREntryNumber entry : mRelevantMMRCoins) {
			if(entry.isEqual(zMMREntryNumber)) {
				return true;
			}
		}
		
		return false;
	}
	
	public ArrayList<Coin> getRelevantCoins(){
		return mComputedRelevantCoins;
	}
	
	public ArrayList<MMREntryNumber> getRelevantCoinsEntries(){
		return mRelevantMMRCoins;
	}
	
	public void removeRelevantCoin(MMREntryNumber zEntry) {
		ArrayList<MMREntryNumber> newRelevantMMRCoins = new ArrayList<>();
		for(MMREntryNumber entry : mRelevantMMRCoins) {
			if(!entry.isEqual(zEntry)) {
				newRelevantMMRCoins.add(entry);
			}
		}
		mRelevantMMRCoins = newRelevantMMRCoins;
	}
	
	public void addChildNode(TxPoWTreeNode zTxPoWTreeNode) {
		//Set the parent
		zTxPoWTreeNode.setParent(this);
		
		//Set the MMR parent
		zTxPoWTreeNode.getMMR().setParent(mMMR);
		
		//Add to the children
		mChildren.add(zTxPoWTreeNode);
	}

	public ArrayList<TxPoWTreeNode> getChildren() {
		return mChildren;
	}

	public void setParent(TxPoWTreeNode zTxPoWTreeNode) {
		mParent = zTxPoWTreeNode;
	}
	
	public TxPoWTreeNode getParent() {
		return mParent;
	}
	
	public TxPoWTreeNode getParent(int zBlocks) {
		TxPoWTreeNode parent = this;
		int counter = 0;
		while(counter<zBlocks && parent.getParent()!=null) {
			parent = parent.getParent();
			counter++;
		}
		
		return parent;
	}
	
	public TxPoWTreeNode getPastNode(MiniNumber zBlockNumber) {
		TxPoWTreeNode parent 	= this;
		while(parent != null) {
			if(parent.getTxPoW().getBlockNumber().isEqual(zBlockNumber)) {
				return parent;
			}
			
			parent = parent.getParent();
		}
		return parent;
	}
	
	/**
	 * Add all the relevant coins from the parent nodes to THIS MMR..
	 */
	public void copyParentRelevantCoins() {
		
		//Some logs
		if(GeneralParams.IS_MEGAMMR) {
			MinimaLogger.log("MMR Parent Copy.. Find relevant coins @ "+getBlockNumber());
		}
		
		//Now copy all the MMR Coins.. 
		ArrayList<Coin> unspentcoins = TxPoWSearcher.getAllRelevantUnspentCoins(getParent());
		
		//We may be adding..
		mMMR.setFinalized(false);
		
		//Some logs
//		if(GeneralParams.IS_MEGAMMR) {
//			MinimaLogger.log("MMR Parent Copy.. cycle through coins : "+unspentcoins.size());
//		}
		
		//copy all of these to the new root..
		for(Coin coin : unspentcoins) {
			
			//Which entry is this
			MMREntryNumber entry = coin.getMMREntryNumber();
			
			//Get the MMRData..
			MMRData data = mMMR.getEntry(0, entry).getMMRData();
			
			//Get the entry..
			MMRProof proof = mMMR.getProofToPeak(coin.getMMREntryNumber());
			
			//Now add it..
			mMMR.updateEntry(entry, proof, data);
			
			//Add to all coins..
			mCoins.add(coin);
			
			//And add this to our list of relevant coins..
			mRelevantMMRCoins.add(entry);
		}
		
		//MMR remains unchanged.. Refinalize..
		mMMR.setFinalized(true);
		
		//Some logs
//		if(GeneralParams.IS_MEGAMMR) {
//			MinimaLogger.log("MMR Parent Copy.. calculate relevant ");
//		}
		
		//Recalculate the relevant coins
		calculateRelevantCoins();
		
		//Some logs
		if(GeneralParams.IS_MEGAMMR) {
//			MinimaLogger.log("MMR Parent Copy.. finished");
			System.gc();
		}
	}
	
	
	public void clearParent() {
		//No TreeNode
		mParent = null;
		
		//No MMR Parent
		mMMR.clearParent();
	}
	
	public void setTotalWeight(BigDecimal zWeight) {
		mTotalWeight = zWeight;
	}
	
	public void addToTotalWeight(BigDecimal zWeight) {
		mTotalWeight = mTotalWeight.add(zWeight);
	}
	
	public BigDecimal getTotalWeight() {
		return mTotalWeight;
	}

	public boolean checkFullTxns(TxPoWDB zTxpDB) {
		
		//If we have already checked 
		if(mHaveCheckedFull) {
			return true;
		}
		
		//Cycle through all the TxPoW and see if we have them all
		ArrayList<MiniData> txns = getTxPoW().getBlockTransactions();
		for(MiniData txn : txns) {
			boolean exists = zTxpDB.exists(txn.to0xString());
			if(!exists) {
				return false;
			}
		}
		
		//We now KNOW we have all the txns
		mHaveCheckedFull = true;
		
		return true;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mTxBlock.writeDataStream(zOut);
		mMMR.writeDataStream(zOut);
		
		int len = mCoins.size();
		MiniNumber.WriteToStream(zOut, len);
		for(Coin cmmr : mCoins) {
			cmmr.writeDataStream(zOut);
		}
		
		len = mRelevantMMRCoins.size();
		MiniNumber.WriteToStream(zOut, len);
		for(MMREntryNumber rel : mRelevantMMRCoins) {
			rel.writeDataStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mChildren 	 		= new ArrayList<>();
		mTotalWeight 		= BigDecimal.ZERO;
		mParent				= null;
		mCoins 				= new ArrayList<>();
		mRelevantMMRCoins 	= new ArrayList<>();
		
		mTxBlock			= TxBlock.ReadFromStream(zIn);
		mMMR				= MMR.ReadFromStream(zIn);
		
		int len = MiniNumber.ReadFromStream(zIn).getAsInt();
		for(int i=0;i<len;i++) {
			mCoins.add(Coin.ReadFromStream(zIn));
		}
		
		len = MiniNumber.ReadFromStream(zIn).getAsInt();
		for(int i=0;i<len;i++) {
			mRelevantMMRCoins.add(MMREntryNumber.ReadFromStream(zIn));
		}
		
		calculateRelevantCoins();
	}
	
	public static TxPoWTreeNode ReadFromStream(DataInputStream zIn) throws IOException {
		TxPoWTreeNode node = new TxPoWTreeNode();
		node.readDataStream(zIn);
		return node;
	}
	
	public static void CheckTxBlockForNotifyCoins(TxBlock zBlock) {
		
		String blockid 		= zBlock.getTxPoW().getTxPoWID();
		String blocknumber 	= zBlock.getTxPoW().getBlockNumber().toString();
		MinimaDB db	 		= MinimaDB.getDB();
		
		//Get all the input coins..
		ArrayList<CoinProof> inputs = zBlock.getInputCoinProofs();
		for(CoinProof cp : inputs) {
			Coin cc = cp.getCoin();
			
			//Check the Coin Notify Details..
			String coinaddress = cc.getAddress().to0xString();
			if(db.checkCoinNotify(coinaddress)) {
				
				//Message..
				JSONObject coinjson = cc.toJSON(true);
				
				//Send a message
				JSONObject data = new JSONObject();
				data.put("address", coinaddress);
				data.put("txblockid", blockid);
				data.put("txblock", blocknumber);
				data.put("spent", true);
				data.put("coin", coinjson);
				
				//MinimaLogger.log("NOTIFY CASCADE COIN : "+data.toString());
				
				//And Post it..
				Main.getInstance().PostNotifyEvent(Main.MAIN_NOTIFYCASCADECOIN, data);
			}
		}
		
		//Get all the output coins
		ArrayList<Coin> outputs = zBlock.getOutputCoins();
		for(Coin cc : outputs) {
			
			//Check the Coin Notify Details..
			String coinaddress = cc.getAddress().to0xString();
			if(db.checkCoinNotify(coinaddress)) {
				
				//Message..
				JSONObject coinjson = cc.toJSON(true);
				
				//Did we remove the state..
				if(!cc.storeState()) {
					//Get it..
					ArrayList<StateVariable> removedstate = zBlock.removedState(cc.getCoinID().to0xString());
					if(removedstate != null) {
						//Add it to the JSON
						coinjson.put("state", Coin.convertStateListToJSON(removedstate));
					}
				}
				
				//Send a message
				JSONObject data = new JSONObject();
				data.put("address", coinaddress);
				data.put("txblockid", blockid);
				data.put("txblock", blocknumber);
				data.put("spent", false);
				data.put("coin", coinjson);
				
				//MinimaLogger.log("NOTIFY CASCADE COIN : "+data.toString());
				
				//And Post it..
				Main.getInstance().PostNotifyEvent(Main.MAIN_NOTIFYCASCADECOIN, data);
			}
		}
	}
}
