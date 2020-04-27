package org.minima.database;

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Hashtable;

import org.minima.GlobalParams;
import org.minima.database.coindb.CoinDB;
import org.minima.database.coindb.CoinDBRow;
import org.minima.database.coindb.java.JavaCoinDB;
import org.minima.database.mmr.MMRData;
import org.minima.database.mmr.MMREntry;
import org.minima.database.mmr.MMRProof;
import org.minima.database.mmr.MMRSet;
import org.minima.database.txpowdb.TxPOWDBRow;
import org.minima.database.txpowdb.TxPowDB;
import org.minima.database.txpowdb.java.JavaDB;
import org.minima.database.txpowtree.BlockTree;
import org.minima.database.txpowtree.BlockTreeNode;
import org.minima.database.txpowtree.MultiLevelCascadeTree;
import org.minima.database.userdb.UserDB;
import org.minima.database.userdb.java.JavaUserDB;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.PubPrivKey;
import org.minima.objects.StateVariable;
import org.minima.objects.Transaction;
import org.minima.objects.TxPOW;
import org.minima.objects.Witness;
import org.minima.objects.base.MMRSumNumber;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniInteger;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.proofs.TokenProof;
import org.minima.system.backup.BackupManager;
import org.minima.system.backup.SyncPackage;
import org.minima.system.backup.SyncPacket;
import org.minima.system.bootstrap.GenesisTxPOW;
import org.minima.system.brains.ConsensusHandler;
import org.minima.system.brains.TxPOWChecker;
import org.minima.system.input.InputHandler;
import org.minima.system.input.functions.gimme50;
import org.minima.system.tx.TXMiner;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.messages.Message;

public class MinimaDB {

	/**
	 * A complete list of all the currently known about TXPOW messages
	 */
	private TxPowDB mTxPOWDB;
	
	/**
	 * A tree representation of the current Minima Block Chain
	 */
	private BlockTree mMainTree;
	
	/**
	 * The Short Term CoinDB database
	 */
	private CoinDB mCoinDB;
	
	/**
	 * The user database with keys, addresses
	 */
	private UserDB mUserDB;
	
	/**
	 * The Backup Manager
	 */
	BackupManager mBackup = null;
	
	/**
	 * Main Constructor
	 */
	public MinimaDB() {
		mTxPOWDB 	= new JavaDB();
		mMainTree 	= new BlockTree();	
		mCoinDB		= new JavaCoinDB();
		mUserDB		= new JavaUserDB();
	}
	
	public void setBackupManager(BackupManager zBackup) {
		mBackup = zBackup;
	}
	
	public BackupManager getBackup() {
		return mBackup;
	}
	
	/**
	 * Set up this DB from GENESIS
	 */ 
	//Genesis txpow
	public void DoGenesis() {
		TxPOW gen = new GenesisTxPOW();
		
		//Add to the list
		TxPOWDBRow row = mTxPOWDB.addTxPOWDBRow(gen);
		row.setOnChainBlock(true);
		row.setInBlockNumber(MiniNumber.ZERO);
		row.setIsInBlock(true);
		row.setBlockState(TxPOWDBRow.TXPOWDBROW_STATE_FULL);
		
		//The initial MMR
		MMRSet base = new MMRSet();
		
		//Add a single zero entry to create the first peak..
		Coin gencoin    = new Coin(new MiniData("0x00"), Address.TRUE_ADDRESS.getAddressData(), MiniNumber.ZERO, Coin.MINIMA_TOKENID);
		MMRData gendata = new MMRData(MiniByte.FALSE, gencoin, MiniNumber.ZERO, new ArrayList<StateVariable>());
		base.addUnspentCoin(gendata);
		
		//Get the root
		gen.setMMRRoot(base.getMMRRoot().getFinalHash());
		gen.setMMRTotal(MMRSumNumber.ZERO);
		
//		SuperBlockLevels.GENESIS_HASH = Crypto.getInstance().hashObject(gen);
		
		//Need to recalculate the TxPOWID
		gen.calculateTXPOWID();
		
		//Genesis root
		BlockTreeNode root = new BlockTreeNode(gen);
		root.setState(BlockTreeNode.BLOCKSTATE_VALID);
		root.setCascade(false);
		
		//Set it..
		root.setMMRset(base);
		
		//Add to the Main Chain
		mMainTree.setTreeRoot(root);
				
		//Back it up..
//		getBackup().backupTxpow(gen); 
//		getBackup().backupFullBlock(gen, new ArrayList<>());
	}
	
	public TxPOW getTxPOW(MiniData zTxPOWID) {
		TxPOWDBRow row = mTxPOWDB.findTxPOWDBRow(zTxPOWID);
		if(row == null) {
			return null;
		}
		return row.getTxPOW();
	}
	
	public boolean isTxPOWFound(MiniData zTxPOWID) {
		return getTxPOW(zTxPOWID)!=null;
	}
	
	public TxPOWDBRow getTxPOWRow(MiniData zTxPOWID) {
		return mTxPOWDB.findTxPOWDBRow(zTxPOWID);
	}
	
	public BlockTreeNode getBlockTreeNode(MiniData zTxPowID) {
		return mMainTree.findNode(zTxPowID);
	}
	
	/**
	 * Process a TXPOW
	 */
	public void processTxPOW(TxPOW zTxPow) {
		//Is it a block.. if so add a BASIC block to the tree
		boolean treeadded = false;
		if(zTxPow.isBlock()) {
			//Try and add to the Block Tree..
			BlockTreeNode node = new BlockTreeNode(zTxPow);
			
			//Add it..
			treeadded = mMainTree.addNode(node);
			
			//Add all the children
			if(treeadded) {
				//Add any children..
				addTreeChildren(zTxPow.getTxPowID());
			}
		}
		
		//Check blocks to see if any are now filled..
		ArrayList<TxPOWDBRow> unfinishedblocks  = mTxPOWDB.getAllBlocksMissingTransactions();
		
		//Cycle through them..
		boolean newfullblock = false;
		for(TxPOWDBRow unblock : unfinishedblocks) {
			boolean allok = true;
			ArrayList<MiniData> txns = unblock.getTxPOW().getBlockTxns();
			for(MiniData txnid : txns) {
				if(getTxPOW(txnid) == null) {
					allok = false;
					break;
				}
			}
			
			//Store for later
			if(allok) {
				//Set the new state..
				unblock.setBlockState(TxPOWDBRow.TXPOWDBROW_STATE_FULL);
				newfullblock = true;
			}
		}
	
		//Do we need to sort out the tree..
		if(treeadded || newfullblock) {
			//get the current tip
			BlockTreeNode tip = mMainTree.getChainTip();
			
			//Now calculate the states of each of the blocks in the tree.. 
			sortBlockTreeNodeStates(mMainTree.getChainRoot(), BlockTreeNode.BLOCKSTATE_VALID);
	
			//Now recalculate the weights only using Valid nodes..
			mMainTree.resetWeights();
			
			//Is there a new tip..
			BlockTreeNode newtip = mMainTree.getChainTip();
			if(newtip.getTxPowID().isEqual(tip.getTxPowID())) {
				//Same tip.. no change..
				return;
			}
			
			//Now cycle down the main chain
			ArrayList<BlockTreeNode> list = null;
			
			//Is it just one block difference
			if(newtip.getParent().getTxPowID().isEqual(tip.getTxPowID())) {
				//Just one block difference.. no need to reset everything..
				list = new ArrayList<>();
				list.add(newtip);
				
			}else{
				//THIS COULD BE MUCH BETTER.. Find the crossover, many optimisations
				
				//Get all the blocks
				list = mMainTree.getAsList();
				
				//Otherwise calculate which TXPOWs are being used
				mTxPOWDB.resetAllInBlocks();
				
				//And Clear the CoinDB
				mCoinDB.clearDB();
			}
			
			//Only add coins from the cascade onwards..
			MiniNumber oldcascade = getMainTree().getCascadeNode().getTxPow().getBlockNumber();
			
			//Reverse the list
			Collections.reverse(list);
			
			//Now sort
			for(BlockTreeNode treenode : list) {
				//Get the Block
				TxPOW txpow = treenode.getTxPow();
				
				//get the row..
				TxPOWDBRow trow = mTxPOWDB.addTxPOWDBRow(txpow);
				
				//What Block
				MiniNumber block = txpow.getBlockNumber();
				
				//Set the details
				trow.setOnChainBlock(true);
				trow.setIsInBlock(true);
				trow.setInBlockNumber(block);
				
				if(treenode.getTxPow().getBlockNumber().isMoreEqual(oldcascade)) {
					//Check for coins in the MMR
					scanMMRSetForCoins(treenode.getMMRSet());
				}
				
				//Now the Txns..
				ArrayList<MiniData> txpowlist = txpow.getBlockTxns();
				for(MiniData txid : txpowlist) {
					trow = mTxPOWDB.findTxPOWDBRow(txid);
					if(trow!=null) {
						//Set that it is in this block
						trow.setOnChainBlock(false);
						trow.setIsInBlock(true);
						trow.setInBlockNumber(block);
					}
				}
			}
			
			//Whats the weight of the tree now..
			BigInteger weight = mMainTree.getChainRoot().getTotalWeight();
			
			if(weight.compareTo(BigInteger.ZERO) == 0) {
				System.out.println("ZERO WEIGHT ERROR!");
			}
			
			/**
			 * Cascade the tree
			 */
			//Create a cascaded version
			MultiLevelCascadeTree casc = new MultiLevelCascadeTree(mMainTree);
			
			//Do it..
			ArrayList<BlockTreeNode> removals = casc.cascadedTree();
			
			//Was it worth it..
			BigInteger cascweight = casc.getCascadeTree().getChainRoot().getTotalWeight();
			
			//See what the difference is..
			BigDecimal ratio = new BigDecimal(cascweight).divide(new BigDecimal(weight), MathContext.DECIMAL128);
			
			//If the cascade levels are long enough this should NEVER happen
			if(ratio.compareTo(new BigDecimal(GlobalParams.MINIMA_CASCADE_RATIO)) < 0) {
				//Too much power lost.. wait..
				MinimaLogger.log("Cascade Tree LOST more than "+GlobalParams.MINIMA_CASCADE_RATIO+" "+ratio);
				//For NOW continue..
//				return;
			}
			
			//Set it
			mMainTree = casc.getCascadeTree();
			
			//Fix the MMR
			BlockTreeNode newcascade  = mMainTree.getCascadeNode();
			if(newcascade != null && newcascade.getMMRSet()!=null){
				//Sort the MMR..
				casc.recurseParentMMR(oldcascade,newcascade.getMMRSet());
			}
			
			//Remove the deleted blocks..
			for(BlockTreeNode node : removals) {
				//We can't keep it..
				TxPOWDBRow row = getTxPOWRow(node.getTxPowID());
				
				//Discard.. no longer an onchain block..
				row.setOnChainBlock(false);
				
				//And delete / move to different folder any file backups..
//				getBackup().deleteTxpow(node.getTxPow());
			}
			
			//Remove all TXPowRows that are less than the cascade node.. they will not be used again..
			MiniNumber cascade 	= mMainTree.getCascadeNode().getTxPow().getBlockNumber();
			
			//Which txpow have been removed..
			ArrayList<TxPOWDBRow> remrows =  mTxPOWDB.removeTxPOWInBlockLessThan(cascade);
			
//			//Remove the deleted txpow..
//			for(TxPOWDBRow remrow : remrows) {
//				getBackup().deleteTxpow(remrow.getTxPOW());
//			}
			
			//Remove all the coins no longer needed.. SPENT
			mCoinDB.removeOldSpentCoins(cascade);
		}
	}
	
	public void scanMMRSetForCoins(MMRSet zMMRSet) {
		if(zMMRSet == null) {
//			System.out.println("Scanning NULL MMRSET..! ");
			return;
		}
		
		//First check the MMR for any relevant coins..
		ArrayList<MMREntry> entries = zMMRSet.getZeroRow();
		for(MMREntry mmrcoin : entries) {
			if(!mmrcoin.getData().isHashOnly()) {
				Coin cc = mmrcoin.getData().getCoin();
				
				//Is the address one of ours..
				boolean rel = getUserDB().isAddressRelevant(cc.getAddress());
					
				//Check the PREV State - could be a KEY we own..
				if(!rel) {
					rel = getUserDB().isStateListRelevant(mmrcoin.getData().getPrevState());
				}
				
				//Keep it if it's relevant
				if(rel) {
					//It's to be kept..
					zMMRSet.addKeeper(mmrcoin.getEntry());
				}
				
				//Do we have it or not..
				CoinDBRow oldrow = getCoinDB().getCoinRow(cc.getCoinID());
				if(oldrow == null && !rel) {
					continue;
				}
				
				//And add to our list..
				CoinDBRow inrow = getCoinDB().addCoinRow(cc);
				
				//Exists already - only want to update if something has changed..
				boolean spent = mmrcoin.getData().isSpent();
				if(!inrow.isInBlock() || inrow.isSpent() != spent) {
					//Update
					inrow.setIsSpent(spent);
					inrow.setIsInBlock(true);
					inrow.setInBlockNumber(zMMRSet.getBlockTime());
					inrow.setMMREntry(mmrcoin.getEntry());
				}
			}
		}
	}
	
	/**
	 * Recursively adds any unaccounted for children
	 * @param zParentID
	 */
	private void addTreeChildren(MiniData zParentID) {
		ArrayList<TxPOWDBRow> unused_children = mTxPOWDB.getChildBlocksTxPOW(zParentID);
		for(TxPOWDBRow txp : unused_children) {
			//We can now add this one..
			mMainTree.addNode(new BlockTreeNode(txp.getTxPOW()));
			
			//And add any other children..
			addTreeChildren(txp.getTxPOW().getTxPowID());
		}
	}
	
	/**
	 * Recursively Sort the blocktreenode states
	 * @param zNode
	 */
	private void sortBlockTreeNodeStates(BlockTreeNode zNode, int zParentState) {
		//Get the txpow row
		TxPOWDBRow row = getTxPOWRow(zNode.getTxPowID());
		
		//Must be a valid parent for anything to happen
		if(zParentState == BlockTreeNode.BLOCKSTATE_INVALID) {
			//All Children are INVALID
			zNode.setState(BlockTreeNode.BLOCKSTATE_INVALID);
		
		}else if(zParentState == BlockTreeNode.BLOCKSTATE_VALID) {
			//Do we check.. only when full
			if(zNode.getState() == BlockTreeNode.BLOCKSTATE_BASIC && row.getBlockState() == TxPOWDBRow.TXPOWDBROW_STATE_FULL) {
				//Need allok for the block to be accepted
				boolean allok = true;
				
				//Check that Block difficulty is Correct!?
				//..TODO
				
				//Check the Super Block Levels are Correct! and point to the correct blocks
				//..TODO
				
				//Create an MMR set that will ONLY be used if the block is VALID..
				MMRSet mmrset = new MMRSet(zNode.getParent().getMMRSet());
				
				//Set this MMR..
				zNode.setMMRset(mmrset);
				
				//Check all the transactions in the block are correct..
				allok = checkFullTxPOW(zNode.getTxPow(), mmrset);
				
				//Check the root MMR..
				if(allok) {
					MiniData root = mmrset.getMMRRoot().getFinalHash();
					if(!row.getTxPOW().getMMRRoot().isEqual(root)) {
						allok = false;	
					}
				}
				
				//if it all passes is OK.. otherwise not ok..
				if(allok) {
					//it's all valid!
					zNode.setState(BlockTreeNode.BLOCKSTATE_VALID);
					
				}else{
					//No good..
					zNode.setState(BlockTreeNode.BLOCKSTATE_INVALID);
				}
			}
		}
		
		//Get all the child blocks..
		ArrayList<BlockTreeNode> children = zNode.getChildren();
		for(BlockTreeNode child : children) {
			sortBlockTreeNodeStates(child, zNode.getState());	
		}
	}
	
	private boolean checkFullTxPOW(TxPOW zBlock, MMRSet zMMRSet) {
		//First check the main transaction..
		if(zBlock.isTransaction()) {
			boolean inputvalid = TxPOWChecker.checkTransactionMMR(zBlock, this, zBlock.getBlockNumber(), zMMRSet,true);
			if(!inputvalid) {
				return false;
			}
		}
		
		//Now cycle through all the transactions in the block..
		ArrayList<MiniData> txns = zBlock.getBlockTxns();
		for(MiniData txn : txns) {
			TxPOWDBRow row = getTxPOWRow(txn);
			TxPOW txpow = row.getTxPOW();
			
			//Check the Proof..
			boolean inputvalid = TxPOWChecker.checkTransactionMMR(txpow, this, zBlock.getBlockNumber(), zMMRSet,true);
			if(!inputvalid) {
				return false;
			}
		}
		
		return true;
	}
	
	/**
	 * Add it if it is not already in the list
	 * 
	 * @param zTxPOW
	 * @return
	 */
	public TxPOWDBRow addNewTxPow(TxPOW zTxPOW) {
		//That's that
		return mTxPOWDB.addTxPOWDBRow(zTxPOW);
	}
	
	
	public boolean isChainRoot() {
		return ( mMainTree.getChainRoot() != null );
	}
	
	public BlockTreeNode hardAddTxPOWBlock(TxPOW zRoot, MMRSet zMMR, boolean zCascade) {
		//Add to the list
		TxPOWDBRow row = mTxPOWDB.addTxPOWDBRow(zRoot);
		row.setIsInBlock(true);
		row.setOnChainBlock(true);
		row.setInBlockNumber(zRoot.getBlockNumber());
		row.setBlockState(TxPOWDBRow.TXPOWDBROW_STATE_FULL);
		
		BlockTreeNode node = new BlockTreeNode(zRoot);
		node.setCascade(zCascade);
		node.setState(BlockTreeNode.BLOCKSTATE_VALID);
		
		//Sort the MMR..
		node.setMMRset(zMMR);

		//Add it..
		mMainTree.hardAddNode(node, true);
		
		return node;
	}
	
	public void hardSetCascadeNode(BlockTreeNode zNode) {
		mMainTree.hardSetCascadeNode(zNode);		
	}
	
	public void hardResetChain() {
		//Cascade it.. and then reset it..
		MultiLevelCascadeTree casc = new MultiLevelCascadeTree(mMainTree);
		casc.cascadedTree();
		mMainTree = casc.getCascadeTree();
	}
	
	public ArrayList<Coin> getTotalSimpleSpendableCoins(MiniData zTokenID) {
		ArrayList<Coin> confirmed   = new ArrayList<>();
		
		MiniNumber top = getTopBlock();
		
		//Check NONE of these are in the mempool.
		ArrayList<Coin> memcoins = getMempoolCoins();
				
		//Do we have any inputs with this address..
		ArrayList<CoinDBRow> relevant = getCoinDB().getComplete();
		for(CoinDBRow row : relevant) {
			if(row.isInBlock() && !row.isSpent() && row.getCoin().getTokenID().isEqual(zTokenID)){
				MiniNumber depth = top.sub(row.getInBlockNumber());
				if(depth.isMoreEqual(GlobalParams.MINIMA_CONFIRM_DEPTH)) {
					//Is this a simple address..
					if(getUserDB().isSimpleAddress(row.getCoin().getAddress())) {
						boolean found = false;
						for(Coin memcoin : memcoins) {
							if(memcoin.getCoinID().isEqual(row.getCoin().getCoinID())) {
								found = true;
								break;
							}
						}
						
						if(!found) {
							confirmed.add(row.getCoin());	
						}	
					}
				}
			}
		}	
		
		return confirmed;
	}
	
	public boolean checkTransactionForMempoolCoins(Transaction zTransaction) {
		ArrayList<Coin> memcoins = getMempoolCoins();
		ArrayList<Coin> inputs = zTransaction.getAllInputs();
		for(Coin in : inputs) {
			for(Coin mem : memcoins) {
				if(in.getCoinID().isEqual(mem.getCoinID())) {
					//No GOOD!
					return true;
				}
			}
		}
		
		return false;
	}
	
	public ArrayList<Coin> getMempoolCoins(){
		ArrayList<Coin> coins = new ArrayList<>();
		
		ArrayList<TxPOWDBRow> rows = getTxPowDB().getAllUnusedTxPOW();
		for(TxPOWDBRow row : rows) {
			TxPOW txpow = row.getTxPOW();
			if(txpow.isTransaction()) {
				ArrayList<Coin> inputs = txpow.getTransaction().getAllInputs();	
				for(Coin cc : inputs) {
					if(!cc.getCoinID().isEqual(gimme50.COINID_INPUT)) {
						coins.add(cc);	
					}
				}
			}
		}
		
		return coins;
	}
	
	public Hashtable<String, MiniNumber> getTotalUnusedAmount() {
		Hashtable<String, MiniNumber> amounts = new Hashtable<>();
		
		ArrayList<TxPOWDBRow> rows = getTxPowDB().getAllUnusedTxPOW();
		for(TxPOWDBRow row : rows) {
			TxPOW txpow = row.getTxPOW();
			if(txpow.isTransaction()) {
				ArrayList<Coin> inputs = txpow.getTransaction().getAllInputs();	
				for(Coin cc : inputs) {
					if(getUserDB().isAddressRelevant(cc.getAddress())) {
						String token = cc.getTokenID().to0xString();
						//Subtract..
						MiniNumber amt = amounts.get(token);
						if(amt == null){
							amt = MiniNumber.ZERO;
						}
						amounts.put(token, amt.sub(cc.getAmount()));
					}
				}
				
				ArrayList<Coin> outputs = txpow.getTransaction().getAllOutputs();	
				for(Coin cc : outputs) {
					if(getUserDB().isAddressRelevant(cc.getAddress())) {
						String token = cc.getTokenID().to0xString();
						//Add..
						MiniNumber amt = amounts.get(token);
						if(amt == null){
							amt = MiniNumber.ZERO;
						}
						amounts.put(token, amt.add(cc.getAmount()));
					}
				}
			}
		}
		
		return amounts;
	}
	
	public Hashtable<String, MiniNumber> getTransactionTokenAmounts(TxPOW zTxPOW) {
		Hashtable<String, MiniNumber> amounts = new Hashtable<>();
	
		if(zTxPOW.isTransaction()) {
			ArrayList<Coin> inputs = zTxPOW.getTransaction().getAllInputs();	
			for(Coin cc : inputs) {
				if(getUserDB().isAddressRelevant(cc.getAddress())) {
					String token = cc.getTokenID().to0xString();
					//Subtract..
					MiniNumber amt = amounts.get(token);
					if(amt == null){
						amt = MiniNumber.ZERO;
					}
					amounts.put(token, amt.sub(cc.getAmount()));
				}
			}
			
			ArrayList<Coin> outputs = zTxPOW.getTransaction().getAllOutputs();	
			for(Coin cc : outputs) {
				if(getUserDB().isAddressRelevant(cc.getAddress())) {
					String token = cc.getTokenID().to0xString();
					//Add..
					MiniNumber amt = amounts.get(token);
					if(amt == null){
						amt = MiniNumber.ZERO;
					}
					amounts.put(token, amt.add(cc.getAmount()));
				}
			}
		}
	
		return amounts;
	}
	
	/**
	 * Create a proofed 
	 * 
	 * @param zAmount
	 * @param zToAddress
	 * @param zChangeAddress
	 * @param zConfirmed
	 * @return
	 */
	public Witness createValidMMRPRoofs(Transaction zTransaction, Witness zWitness) {
		//The Base current MMRSet
		MMRSet basemmr  = getMainTree().getChainTip().getMMRSet();
		
		//What Block are we on..
		MiniNumber currentblock = basemmr.getBlockTime();
		
		//Clear the proofs..
		zWitness.clearMMRProofs();
		
		//Cycle through the inputs..
		ArrayList<Coin> ins = zTransaction.getAllInputs();
		
		//What's the most recent coin used..
		MiniNumber recent = null;
		for(Coin cc : ins) {
			MiniNumber inblock = null;
					
			//Get the block..
			CoinDBRow crow = getCoinDB().getCoinRow(cc.getCoinID());
			if(crow != null) {
				inblock = crow.getInBlockNumber();
			}else {
				//Search for the coin..
				MMREntry entry =  basemmr.findEntry(cc.getCoinID());
				inblock = entry.getData().getInBlock();
			}
			
			if(recent == null) {
				recent = inblock;
			}else {
				if(inblock.isMore(recent)) {
					recent = inblock; 
				}
			}
		}
		
		//Which MMRSet to use.. use the same one for the wholetransaction
		MiniNumber howdeep = currentblock.sub(recent);
		
		//MAX 64 blocks in the past should be fine.. so reorgs won't invalidate it..
		if(howdeep.isMore(MiniNumber.SIXTYFOUR)) {
			howdeep = MiniNumber.SIXTYFOUR;
		}
	
		//The Actual MMR block we will use..
		MMRSet proofmmr = basemmr.getParentAtTime(currentblock.sub(howdeep));
		
		//Now add the actual MMR Proofs..
		for(Coin cc : ins) {
			MiniInteger entrynum = null;
			
			//Get the entry
			CoinDBRow crow = getCoinDB().getCoinRow(cc.getCoinID());
			if(crow != null) {
				entrynum = crow.getMMREntry();
			}else {
				entrynum = proofmmr.findEntry(cc.getCoinID()).getEntry();
			}
			
			//Get a proof from a while back.. more than confirmed depth, less than cascade
			MMRProof proof = proofmmr.getProof(entrynum);
			
			//Hmm.. this should not happen
			if(proof == null) {
				return null;
			}
			
			//Add the proof for this coin..
			zWitness.addMMRProof(proof);
		}
		
		return zWitness;
	}
	
	/**
	 * Create both the transaction and the witness data
	 * 
	 * @param zAmount
	 * @param zToAddress
	 * @param zChangeAddress
	 * @param zConfirmed
	 * @return
	 */
	public Message createTransaction(MiniNumber zAmount, Address zToAddress, 
									 Address zChangeAddress, 
									 ArrayList<Coin> zConfirmed,
									 MiniData zTokenID,
									 MiniData zChangeTokenID,
									 TokenProof zTokenGen) {
		
		return createTransaction(zAmount, zToAddress, zChangeAddress, 
				zConfirmed, zTokenID, zChangeTokenID, zTokenGen, new Transaction());
	}
		
	public Message createTransaction(MiniNumber zAmount, Address zToAddress, 
				 Address zChangeAddress, 
				 ArrayList<Coin> zConfirmed,
				 MiniData zTokenID,
				 MiniData zChangeTokenID,
				 TokenProof zTokenGen, Transaction zUseThisTransaction) {
		
		//The Transaction - couls already have some state variables set.. TXN_AUTO etc..
		Transaction trx = zUseThisTransaction;
		Witness wit 	= new Witness();
		
		//Which signatures are required
		ArrayList<MiniData> sigpubk = new ArrayList<>();
		
		//Sort the iputs
		MiniNumber currentin = new MiniNumber();
		for(Coin cc : zConfirmed) {
			if(currentin.isLess(zAmount)) {
				//Make sure script is set
				String script = getUserDB().getScript(cc.getAddress());
				if(script.equals("")) {
					System.out.println("ERROR UNKNOWN ADDRESS "+cc.getAddress()+" not in database..");
					return null;
				}
				
				//Add this coin to the inputs..
				trx.addInput(cc);
				try {
					wit.addScript(script, cc.getAddress().getLength()*8);
				} catch (Exception e) {
					MinimaLogger.log("Invalid Script.. "+script);
					return null;
				}
								
				//And finally sign!
				MiniData pubk = getUserDB().getPublicKeyForSimpleAddress(cc.getAddress());
				
				//Add to list of signatures..
				sigpubk.add(pubk);
				
				//And the total
				currentin = currentin.add(cc.getAmount());
			}else {
				break;
			}
		}
		
		//Double check..
		if(currentin.isLess(zAmount)) {
			//ERROR!
			MinimaLogger.log("ERROR Insufficient funds!! "+currentin);
			return null;
		}
		
		//And now the Outputs - one for the recipient
		Coin out = new Coin(Coin.COINID_OUTPUT,zToAddress.getAddressData(),zAmount,zTokenID);
		trx.addOutput(out);
		
		//And one for change
		MiniNumber change  = currentin.sub(zAmount);
		if(!change.isEqual(MiniNumber.ZERO)) {
			Coin chg = new Coin(Coin.COINID_OUTPUT, zChangeAddress.getAddressData(), change, zChangeTokenID);
			trx.addOutput(chg);	
		}
		
		//And FINALLY - Is there Token generation
		if(zTokenGen != null) {
			trx.setTokenGenerationDetails(zTokenGen);
		}
		
		//Check all the inputs..
		if(!trx.checkValidInOutPerToken()) {
			//ERROR!
			MinimaLogger.log("ERROR Inputs and Outpus do not add up! "+trx);
			return null;
		}
		
		//And Now add the Proofs..
		Witness witcheck = createValidMMRPRoofs(trx, wit);
		if(witcheck == null) {
			//ERROR!
			MinimaLogger.log("ERROR MMR Proofs could not be found for transaction "+trx);
			return null;
		}
		
		//Now we have a full transaction we can sign it!
		MiniData transhash = Crypto.getInstance().hashObject(trx);
		for(MiniData pubk : sigpubk) {
			//Get the Pub Priv..
			PubPrivKey signer = getUserDB().getPubPrivKey(pubk);
			
			//Sign the data
			MiniData signature = signer.sign(transhash);
			
			//Add to the witness..
			wit.addSignature(pubk, signature);	
		}
				
		//The return package
		Message ret = new Message(ConsensusHandler.CONSENSUS_SENDTRANS);
		ret.addObject("transaction", trx);
		ret.addObject("witness", wit);
		
		return ret;
	}
	
	/**
	 * Get OUR current next TXPOW!
	 * 
	 * @param zTrans
	 * @param zWitness
	 * @return
	 */
	public TxPOW getCurrentTxPow(Transaction zTrans, Witness zWitness, JSONArray zContractLogs) {
		//Get the current best block..
		BlockTreeNode tip = mMainTree.getChainTip();
		
		//TODO - Add Burn Transaction and Witness!
		
		//Fresh TxPOW
		TxPOW txpow = new TxPOW();
				
		//Set the time
		txpow.setTimeSecs(new MiniNumber(""+(System.currentTimeMillis()/1000)));
			
		//Set the Transaction..
		txpow.setTransaction(zTrans);
		txpow.setWitness(zWitness);
		
		//The Transaction Difficulty is set by the user after testing.. 
		//He performs 10 seconds of work..
		txpow.setTxDifficulty(TXMiner.BASE_TXN);
		txpow.setBlockDifficulty(TXMiner.BASE_BLOCK);
		
		//Block Details..
		txpow.setBlockNumber(tip.getTxPow().getBlockNumber().increment());
		
		//Previous block
		txpow.setParent(tip.getTxPowID());
		
		if(!GlobalParams.MINIMA_ZERO_DIFF_BLK) {
			//Calculate New Chain Speed
			int len = mMainTree.getAsList().size();
			 
			if(len > GlobalParams.MINIMA_CASCADE_START_DEPTH ) {
				//Desired Speed.. in blocks per second
				MiniNumber actualspeed 	= mMainTree.getChainSpeed();
				
				//Calculate the speed ratio
				MiniNumber speedratio   = GlobalParams.MINIMA_BLOCK_SPEED.div(actualspeed);
				
				//Current avg
				BigInteger avgdiff = mMainTree.getAvgChainDifficulty();
				BigDecimal avgdiffdec = new BigDecimal(avgdiff);
				
				//Mutily by the ratio
				BigDecimal newdiffdec = avgdiffdec.multiply(speedratio.getAsBigDecimal());
				BigInteger newdiff    = newdiffdec.toBigInteger();
				
				//Check if more than maximum..
				if(newdiff.compareTo(Crypto.MAX_VAL)>0) {
					newdiff = Crypto.MAX_VAL;
				}
				//Create the hash
				MiniData diffhash = new MiniData("0x"+newdiff.toString(16)); 
				
				//Now set the new difficulty
				txpow.setBlockDifficulty(diffhash);
			}
		}
		
		//Super Block Levels..
		for(int i=0;i<GlobalParams.MINIMA_CASCADE_LEVELS;i++) {
			txpow.mSuperParents[i] = tip.getTxPow().mSuperParents[i];
		}

		//And now set the correct SBL given the last block
		int sbl = tip.getSuperBlockLevel();
		
		//All levels below this now point to the last block..
		MiniData tiptxid = tip.getTxPowID();
		for(int i=sbl;i>=0;i--) {
			txpow.mSuperParents[i] = tiptxid;
		}			

		//Get the current MMRSet
		MMRSet newset = new MMRSet(tip.getMMRSet());
		
		//Check the first transaction
		if(!zTrans.isEmpty()) {
			boolean valid = TxPOWChecker.checkTransactionMMR(zTrans, zWitness, this, 
					txpow.getBlockNumber(),newset,true, zContractLogs);
			
			//MUST be valid.. ?
			if(!valid) {
//				MinimaLogger.log("ERROR: Your own transaction is invalid !?");
				return null;
			}
		}
		
		//Set the current Transaction List!
		ArrayList<TxPOWDBRow> unused = mTxPOWDB.getAllUnusedTxPOW();
		for(TxPOWDBRow row : unused) {
			//Check is still VALID..
			TxPOW txp = row.getTxPOW();
			
			/**
			 * MUST be a transaction as that prevents double entry. A block with no transaction 
			 * is valid.. but no way to check it has already been added
			 */
			if(txp.isTransaction()) {
				boolean valid = TxPOWChecker.checkTransactionMMR(txp, this, txpow.getBlockNumber(),newset,true);
				
				if(valid) {
					//Add it..
					txpow.addBlockTxPOW(txp);	
				
				}else {
					//Remove this!.. It WAS valid but now not.. :(.. dump it..
					mTxPOWDB.removeTxPOW(txp.getTxPowID());
					
					//And delete..
					getBackup().deleteTxpow(txp);
					
					MinimaLogger.log("Removing invalid TXPOW.. "+txp);
				}
			}
		}
		
		//Set the current MMR
		MMRData root = newset.getMMRRoot();
		txpow.setMMRRoot(root.getFinalHash());
		txpow.setMMRTotal(root.getValueSum());
		
		//And return..
		return txpow;
	}
	
	/**
	 * Get the current top block number
	 * 
	 * @return The Block Number
	 */
	public MiniNumber getTopBlock() {
		return getMainTree().getChainTip().getTxPow().getBlockNumber();
	}
	
	/**
	 * Get the Current IBD - total required to log in for a new user
	 */
	public int getIntroSyncSize() {
		SyncPackage sp = new SyncPackage();
		
		//Lets create a sync package
		ArrayList<BlockTreeNode> nodes = getMainTree().getAsList();
		MiniNumber casc = getMainTree().getCascadeNode().getTxPow().getBlockNumber();
		sp.setCascadeNode(casc);
		
		//Cycle through it all..
		for(BlockTreeNode node : nodes) {
			MiniNumber block = node.getTxPow().getBlockNumber();
			sp.getAllNodes().add(0,new SyncPacket(node, block.isLessEqual(casc)));
		}
		
		//Write it out..
		try {
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			DataOutputStream dos = new DataOutputStream(baos);
			sp.writeDataStream(dos);
			dos.flush();
			dos.close();
			
			return baos.toByteArray().length;
			
		}catch(Exception exc) {
			exc.printStackTrace();
		}
		
		return 0;
	}
	
	/**
	 * Extra Functions
	 */
	public BlockTree getMainTree() {
		return mMainTree;
	}
	
	public TxPowDB getTxPowDB() {
		return mTxPOWDB;
	}
	
	public CoinDB getCoinDB() {
		return mCoinDB;
	}
	
	public UserDB getUserDB() {
		return mUserDB;
	}
	
	public void setUserDB(JavaUserDB zJDB) {
		mUserDB = zJDB;
	}
}
