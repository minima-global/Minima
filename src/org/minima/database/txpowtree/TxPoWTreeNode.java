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
import org.minima.database.wallet.KeyRow;
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
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;

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
	
	private TxPoWTreeNode() {}
	
	public TxPoWTreeNode(TxBlock zTxBlock) {
		this(zTxBlock, true);
	}
	
	public TxPoWTreeNode(TxBlock zTxBlock, boolean zFindRelevant) {
		mTxBlock		= zTxBlock;
		mChildren 	 	= new ArrayList<>();
		mTotalWeight 	= BigDecimal.ZERO;
		mParent			= null;
		
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
		Wallet wallet = MinimaDB.getDB().getWallet();
		
		//Are we checking for relevant data
		ArrayList<KeyRow> allrel = new ArrayList<>();
		if(zFindRelevant) {
			allrel = wallet.getAllRelevant(false);
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

			//Get the Hash of this 
			MiniData hashspent = Crypto.getInstance().hashObject(spentcoin);
			
			//And create a new MMRData structure - with ZERO value as it is now spent
			MMRData mmrdata = new MMRData(hashspent, MiniNumber.ZERO);
			
			//Update the MMR
			mMMR.updateEntry(entrynumber, input.getMMRProof(), mmrdata);
			
			//Add to the total List of coins fro this block
			mCoins.add(spentcoin);
			
			//Is this Relevant to us..
			if(checkRelevant(spentcoin, allrel)) {
				mRelevantMMRCoins.add(entrynumber);
				
				//Message..
				MinimaLogger.log("NEW Spent Coin : "+spentcoin.toJSON());
				balancechange = true;
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
			
			//Get the Hash of this 
			MiniData hashunspent = Crypto.getInstance().hashObject(newcoin);
			
			//And create a new MMRData with the correct amount
			MMRData mmrdata = new MMRData(hashunspent, output.getAmount());
			
			//And add to the MMR
			mMMR.addEntry(mmrdata);	
			
			//Add to the total List of coins for this block
			mCoins.add(newcoin);
			
			//Is this Relevant to us..
			if(checkRelevant(output, allrel)) {
				mRelevantMMRCoins.add(entrynumber);
				
				//Message..
				MinimaLogger.log("NEW Unspent Coin : "+newcoin.toJSON());
				balancechange = true;
			}
		}
		
		//All done..!
		mMMR.finalizeSet();
		
		//Calculate the Relevant Coins..
		if(zFindRelevant) {
			calculateRelevantCoins();
		}
		
		//Has the balance changed
		if(balancechange) {
			Main.getInstance().PostMessage(Main.MAIN_BALANCE);
		}
	}
	
	/**
	 * Check if this coin is relevant to this wallet and therefore should be kept
	 */
	private boolean checkRelevant(Coin zCoin, ArrayList<KeyRow> zAllRelevant) {
		
		//Cycle through ALL the wallet entries..
		for(KeyRow wk : zAllRelevant) {
			
			//Is the address one of ours..
			if(wk.trackAddress() && zCoin.getAddress().to0xString().equals(wk.getAddress())) {
				return true;
			}
			
			//Are any of the state variables relevant to us..
			ArrayList<StateVariable> state = zCoin.getState();
			for(StateVariable sv : state) {
				
				if(sv.getType().isEqual(StateVariable.STATETYPE_HEX)) {
					String svstr = sv.toString();
					
					//Custom scripts have no public key..
					if(!wk.getPublicKey().equals("") && svstr.equals(wk.getPublicKey())) {
						return true;
					}else if(wk.trackAddress() && svstr.equals(wk.getAddress())){
						return true;
					}
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
			
			//Cycle the relevant
			for(MMREntryNumber relentry : mRelevantMMRCoins) {
				
				if(coin.getMMREntryNumber().isEqual(relentry)) {
					mComputedRelevantCoins.add(coin);
					break;
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
		
		//Now copy all the MMR Coins.. 
		ArrayList<Coin> unspentcoins = TxPoWSearcher.getAllRelevantUnspentCoins(getParent());
		
		//We may be adding..
		mMMR.setFinalized(false);
		
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
		
		//Recalculate the relevant coins
		calculateRelevantCoins();
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
}
