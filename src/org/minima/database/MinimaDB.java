package org.minima.database;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.StringTokenizer;

import org.minima.GlobalParams;
import org.minima.database.coindb.CoinDB;
import org.minima.database.coindb.CoinDBRow;
import org.minima.database.coindb.java.FastCoinDB;
import org.minima.database.mmr.MMRData;
import org.minima.database.mmr.MMREntry;
import org.minima.database.mmr.MMREntryDB;
import org.minima.database.mmr.MMRProof;
import org.minima.database.mmr.MMRSet;
import org.minima.database.txpowdb.TxPOWDBRow;
import org.minima.database.txpowdb.TxPowDB;
import org.minima.database.txpowdb.java.FastJavaDB;
import org.minima.database.txpowtree.BlockTree;
import org.minima.database.txpowtree.BlockTreeNode;
import org.minima.database.txpowtree.CascadeTree;
import org.minima.database.userdb.UserDB;
import org.minima.database.userdb.java.JavaUserDB;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.StateVariable;
import org.minima.objects.Transaction;
import org.minima.objects.TxPoW;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.greet.SyncPackage;
import org.minima.objects.greet.SyncPacket;
import org.minima.objects.keys.MultiKey;
import org.minima.objects.proofs.TokenProof;
import org.minima.system.brains.BackupManager;
import org.minima.system.brains.ConsensusHandler;
import org.minima.system.input.functions.gimme50;
import org.minima.system.txpow.GenesisTxPOW;
import org.minima.system.txpow.TxPoWChecker;
import org.minima.system.txpow.TxPoWMiner;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.ObjectStack;
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
	 * When you mine.. You can't use these INPUTS in your transactions
	 */
	Hashtable<String,Transaction> mMiningTransactions = new Hashtable<>();
	
	/**
	 * Main Constructor
	 */
	public MinimaDB() {
		//Use the new FAST TxPoWDB
//		mTxPOWDB 	= new JavaDB();
		mTxPOWDB 	= new FastJavaDB();
		
		mMainTree 	= new BlockTree();	

		//New FAST CoinDB
//		mCoinDB		= new JavaCoinDB();
		mCoinDB		= new FastCoinDB();
		
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
		//The Gensis TxPoW
		TxPoW gen = new GenesisTxPOW();
		
		//The initial MMR
		MMRSet base = new MMRSet();
		
		//Add a single zero entry to create the first peak..
		Coin gencoin    = new Coin(new MiniData("0x00"), Address.TRUE_ADDRESS.getAddressData(), MiniNumber.ZERO, Coin.MINIMA_TOKENID);
		MMRData gendata = new MMRData(MiniByte.FALSE, gencoin, MiniNumber.ZERO, new ArrayList<StateVariable>());
		base.addUnspentCoin(gendata);
		
		//Get the root
		gen.setMMRRoot(base.getMMRRoot().getFinalHash());
		gen.setMMRTotal(MiniNumber.ZERO);
		
		//Will make this PERMANENT in future release.. so ALL roads lead back to it..
		//SuperBlockLevels.GENESIS_HASH = Crypto.getInstance().hashObject(gen);
		
		//Need to recalculate the TxPOWID
		gen.calculateTXPOWID();
		
		//Add to the list now that TxPoWID is set
		TxPOWDBRow row = mTxPOWDB.addTxPOWDBRow(gen);
		row.setMainChainBlock(true);
		row.setInBlockNumber(MiniNumber.ZERO);
		row.setIsInBlock(true);
		row.setBlockState(TxPOWDBRow.TXPOWDBROW_STATE_FULL);
		
		//Genesis root
		BlockTreeNode root = new BlockTreeNode(gen);
		root.setState(BlockTreeNode.BLOCKSTATE_VALID);
		root.setCascade(false);
		
		//Set it..
		root.setMMRset(base);
		
		MinimaLogger.log("Genesis TxPoW : "+gen.getTxPowID().to0xString());
		
		//Add to the Main Chain
		mMainTree.setTreeRoot(root);
				
		//Back it up..
		getBackup().backupTxpow(gen); 
	}
	
	public TxPoW getTxPOW(MiniData zTxPOWID) {
		TxPOWDBRow row = mTxPOWDB.findTxPOWDBRow(zTxPOWID);
		if(row == null) {
			return null;
		}
		return row.getTxPOW();
	}
	
	public TxPOWDBRow getTxPOWRow(MiniData zTxPOWID) {
		return mTxPOWDB.findTxPOWDBRow(zTxPOWID);
	}
	
	/**
	 * Process a TXPOW
	 */
	public void processTxPOW(TxPoW zTxPow) {
		//Is it a block.. if so add a BASIC block to the tree
		boolean treeadded = false;
		
		//A NULL txpow means do an update whatever..
		if(zTxPow != null) {
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
		}else{
			treeadded = true;	
		}
		
		//Check blocks to see if any are now filled..
		ArrayList<TxPOWDBRow> unfinishedblocks  = mTxPOWDB.getAllBlocksMissingTransactions();
		
		//Cycle through them..
		boolean newfullblock = false;
		for(TxPOWDBRow unblock : unfinishedblocks) {
			boolean allok = true;
			ArrayList<MiniData> txns = unblock.getTxPOW().getBlockTransactions();
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
			
			//Get the OLD chain..
			ArrayList<BlockTreeNode> oldlist = mMainTree.getAsList();
			
			//Now calculate the states of each of the blocks in the tree.. 
			mMainTree.sortBlockTreeNodeStates(this);
	
			//Now recalculate the weights only using Valid nodes..
			mMainTree.resetWeights();
			
			//Is there a new tip..
			BlockTreeNode newtip = mMainTree.getChainTip();
			if(newtip.getTxPowID().isEqual(tip.getTxPowID())) {
				//Same tip.. no change..
				return;
			}
			
			//Now cycle down the main chain
			ArrayList<BlockTreeNode> list = new ArrayList<>();
			
			//Find how far back to cull the db coins - where is the crossover..
			BlockTreeNode currentblock = newtip;
			MiniNumber lastblock       = MiniNumber.ZERO;
			boolean found              = false;
			while(currentblock!=null && !found) {
				//Add to the list
				list.add(0,currentblock);
				
				//Search for a crossover
				for(BlockTreeNode node : oldlist) {
					//Check if the same..
					if(node.getTxPowID().isEqual(currentblock.getTxPowID())) {
						//Found crossover
						lastblock = currentblock.getBlockNumber();
						found = true;
						break;
				
					}else if(node.getBlockNumber().isLess(currentblock.getBlockNumber())) {
						//No way back..
						break;
					}
				}
				
				//None found go deeper..
				currentblock = currentblock.getParent();
			}
			
			//Reset transaction from that block onwards
			mTxPOWDB.resetBlocksFromOnwards(lastblock);
			
			//Reset coins from that block onwards
			mCoinDB.resetCoinsFomOnwards(lastblock);
			
			//Now sort
			for(BlockTreeNode treenode : list) {
				//Get the Block
				TxPoW txpow = treenode.getTxPow();
		
				//Get the database txpow..
				TxPOWDBRow trow = mTxPOWDB.findTxPOWDBRow(txpow.getTxPowID());
				
				//What Block
				MiniNumber block = txpow.getBlockNumber();
				
				//Set the details
				trow.setMainChainBlock(true);
				trow.setIsInBlock(true);
				trow.setInBlockNumber(block);
				
				//Check for coins in the MMR
				scanMMRSetForCoins(treenode.getMMRSet());
				
				//Now the Txns..
				ArrayList<MiniData> txpowlist = txpow.getBlockTransactions();
				for(MiniData txid : txpowlist) {
					trow = mTxPOWDB.findTxPOWDBRow(txid);
					if(trow!=null) {
						//Set that it is in this block
						trow.setMainChainBlock(false);
						trow.setIsInBlock(true);
						trow.setInBlockNumber(block);
					}
				}
			}
			
			/**
			 * Only cascade the tree every 100 blocks.. no need to do it EVERY block..
			 */
			MiniNumber checkcasc = newtip.getBlockNumber().modulo(GlobalParams.MINIMA_CASCADE_FREQUENCY);
			if(!checkcasc.isEqual(MiniNumber.ZERO)) {
				return;
			}
			
			/**
			 * Cascade the tree
			 */
			CascadeTree casc = new CascadeTree(mMainTree);
			
			//Cascade the tree
			casc.cascadedTree();
			
			//Get the removals
			ArrayList<BlockTreeNode> removals = casc.getRemoved();
			
			//Get the Tree
			mMainTree = casc.getCascadeTree();
			
			//Remove the deleted blocks..
			for(BlockTreeNode node : removals) {
				//We can't keep it..
				TxPOWDBRow row = getTxPOWRow(node.getTxPowID());
				
				//Discard.. no longer an on chain block..
				row.setMainChainBlock(false);
				
				//And delete / move to different folder any file backups..
				getBackup().deleteTxpow(node.getTxPow());
			}
			
			//Remove all TXPowRows that are less than the cascade node.. they will not be used again..
			MiniNumber cascade 	= mMainTree.getCascadeNode().getBlockNumber();
			
			//Update the MMREntryDB to remove unneeded MMR data..
			MMREntryDB.getDB().cleanUpDB(cascade);
			
			//Which txpow have been removed..
			ArrayList<TxPOWDBRow> remrows =  mTxPOWDB.removeTxPOWInBlockLessThan(cascade);
			
			//Remove the deleted txpow..
			for(TxPOWDBRow remrow : remrows) {
				getBackup().deleteTxpow(remrow.getTxPOW());
			}
			
			//Remove all the coins no longer needed.. SPENT
			mCoinDB.removeOldSpentCoins(cascade);
			
			//Clean up..
			System.gc();
		}
	}
	
	public void scanMMRSetForCoins(MMRSet zMMRSet) {
		//Check the MMR for any coins..
		ArrayList<MMREntry> entries = zMMRSet.getZeroRow();
		for(MMREntry mmrcoin : entries) {
			if(!mmrcoin.getData().isHashOnly()) {
				//Get the Coin..
				Coin cc = mmrcoin.getData().getCoin();
				
				//Is it spent
				boolean spent = mmrcoin.getData().isSpent();
				
				//Is the address one of ours..
				boolean rel = getUserDB().isCoinRelevant(cc);
					
				//Check the PREV State - could be a KEY or ADDRESS we own..
				if(!rel) {
					rel = getUserDB().isStateListRelevant(mmrcoin.getData().getPrevState());
				}
				
				//Keep it if it's relevant
				if(rel) {
					//It's to be kept in the MMR past the cascade..
					zMMRSet.addKeeper(mmrcoin.getEntryNumber());
				}
				
				//Add to our list - or return the already existing  version..
				CoinDBRow inrow = getCoinDB().addCoinRow(cc);
				inrow.setRelevant(rel);
				
				//Exists already - only want to update if something has changed..
				//Same coin can be in the MMR for multiple blocks.. only do this ONCHANGE
				if(!inrow.isInBlock() || inrow.isSpent() != spent) {
					inrow.setIsSpent(spent);
					inrow.setIsInBlock(true);
					inrow.setInBlockNumber(zMMRSet.getBlockTime());
					inrow.setMMREntry(mmrcoin.getEntryNumber());
				}
			}
		}
	}
	
	/**
	 * Recursively adds any unaccounted for children
	 * @param zParentID
	 */
	public void addTreeChildren(MiniData zParentID) {
		/**
		 * Recursive.. BAD
		 */
//		ArrayList<TxPOWDBRow> unused_children = mTxPOWDB.getChildBlocksTxPOW(zParentID);
//		for(TxPOWDBRow txp : unused_children) {
//			//We can now add this one..
//			mMainTree.addNode(new BlockTreeNode(txp.getTxPOW()));
//			
//			//And add any other children..
//			addTreeChildren(txp.getTxPOW().getTxPowID());
//		}
		
		/**
		 * NON-RECURSIVE METHOD..
		 */
		//Create a new stack of block ids to check..
		ObjectStack stack = new ObjectStack();
		
		//Add the initial ID
		stack.push(zParentID);
		
		//Keep going until everything is checked
		while(!stack.isEmpty()) {
			//Get the ID
			MiniData parentid = (MiniData) stack.pop();
				
			//Get the children
			ArrayList<TxPOWDBRow> children = mTxPOWDB.getChildBlocksTxPOW(parentid);
				
			//Add the children
			for(TxPOWDBRow txp : children) {
				//We can now add this one..
				boolean added = mMainTree.addNode(new BlockTreeNode(txp.getTxPOW()));
				
				//Only if it works!
		        if (added) {
		        	stack.push(txp.getTxPOW().getTxPowID());	
		        }
			}
		}
	}
	
	public TxPoW findBlockForTransaction(TxPoW zTxPoWTransaction) {
		ArrayList<BlockTreeNode> nodes = getMainTree().getAsList(true);
		for(BlockTreeNode node : nodes) {
			//Get the TxPoW
			TxPoW chaintxpow = node.getTxPow();
			ArrayList<MiniData> txns = chaintxpow.getBlockTransactions();
			for(MiniData txn : txns) {
				if(txn.isEqual(zTxPoWTransaction.getTxPowID())) {
					return chaintxpow;
				}
			}
		}	
		return null;
	}
	
	
	public boolean checkAllTxPOW(BlockTreeNode zNode, MMRSet zMMRSet) {
		//The TxPoW to check..
		TxPoW nodetxp = zNode.getTxPow();

		//Txn Number.. unique for every transaction
		MiniNumber txncounter = MiniNumber.ZERO;
		
		//First check the main transaction..
		if(nodetxp.isTransaction()) {
			boolean inputvalid = TxPoWChecker.checkTransactionMMR(nodetxp, this, nodetxp, zMMRSet,true);
			if(!inputvalid) {
				return false;
			}
		}
		
		//Now cycle through all the transactions in the block..
		ArrayList<MiniData> txns = nodetxp.getBlockTransactions();
		for(MiniData txn : txns) {
			TxPOWDBRow row = getTxPOWRow(txn);
			TxPoW txpow    = row.getTxPOW();
			
			//Check the Proof.. - after a sync some txpow are assume valid..
			if(!row.isAssumeValid()) {
				txncounter = txncounter.increment();
				boolean inputvalid = TxPoWChecker.checkTransactionMMR(txpow, this, nodetxp, zMMRSet,true);
				if(!inputvalid) {
					return false;
				}
			}
			
			//Is it a block with no transaction..
			if(txpow.isBlock() && !txpow.isTransaction()) {
				//Check with limits..
				MiniNumber diff = txpow.getBlockNumber().sub(nodetxp.getBlockNumber()).abs();
				if(diff.isMoreEqual(MiniNumber.EIGHT)) {
					MinimaLogger.log("Block too far to be included in block "+diff+" / 8 max.. \nNODE :"+zNode+"\nTXPOW:"+txpow);
					return false;
				}
				
				//Check the parents
				BlockTreeNode parent = zNode.getParent();
				for(int i=0;i<8;i++) {
					//Check..
					if(parent.checkForTxpow(txpow.getTxPowID())) {
						MinimaLogger.log("Block in Parent Block Allready ["+i+"] .. \nNODE :"+parent+"\nTXPOW:"+txpow);
						return false;
					}
					
					//Get the next..
					parent = parent.getParent();
					if(parent == null) {
						break;
					}
				}
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
	public TxPOWDBRow addNewTxPow(TxPoW zTxPOW) {
		//That's that
		return mTxPOWDB.addTxPOWDBRow(zTxPOW);
	}
	
	public BlockTreeNode hardAddTxPOWBlock(TxPoW zTxPoW, MMRSet zMMR, boolean zCascade) {
		//Add to the list
		TxPOWDBRow row = mTxPOWDB.addTxPOWDBRow(zTxPoW);
		row.setMainChainBlock(true);
		row.setIsInBlock(true);
		row.setInBlockNumber(zTxPoW.getBlockNumber());
		row.setBlockState(TxPOWDBRow.TXPOWDBROW_STATE_FULL);
		
		BlockTreeNode node = new BlockTreeNode(zTxPoW);
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
		//Recalculate the weights
		mMainTree.resetWeights();
		
		//Cascade it.. and then reset it..
		CascadeTree casc = new CascadeTree(mMainTree);
		casc.cascadedTree();
		mMainTree = casc.getCascadeTree();
	}
	
	
	/**
	 * Clear the TxPoWDB so that only valid TxPOW in the main chain are kept
	 * 
	 * Use after a re-sync
	 */
	public void resetAllTxPowOnMainChain(){
		//And Now sort the TXPOWDB
		ArrayList<BlockTreeNode> list = getMainTree().getAsList();
		getTxPowDB().resetAllInBlocks();
		
		//Now sort
		for(BlockTreeNode treenode : list) {
			//Get the Block
			TxPoW txpow = treenode.getTxPow();
			
			//What Block
			MiniNumber block = txpow.getBlockNumber();
			
			//Set the main chain details..
			TxPOWDBRow blockrow = getTxPowDB().findTxPOWDBRow(txpow.getTxPowID());
			blockrow.setInBlockNumber(block);
			blockrow.setMainChainBlock(true);
			blockrow.setIsInBlock(true);
			
			//Now the Txns..
			ArrayList<MiniData> txpowlist = txpow.getBlockTransactions();
			for(MiniData txid : txpowlist) {
				TxPOWDBRow trow = getTxPowDB().findTxPOWDBRow(txid);
				if(trow!=null) {
					//Set that it is in this block
					trow.setMainChainBlock(false);
					trow.setIsInBlock(true);
					trow.setInBlockNumber(block);
				}
			}
		}
	}
	
	/**
	 * When you mine a transaction these must be taken 
	 * into account from coin selection
	 * 
	 * Return true if is a NEW transaction..
	 */
	public boolean addMiningTransaction(Transaction zTrans) {
		//Hash it..
		MiniData transhash = Crypto.getInstance().hashObject(zTrans, 160);
		String hash        = transhash.to0xString();
		
		//Do we have it..
		Transaction prev = mMiningTransactions.get(hash);
		if(prev!=null) {
			return false;
		}
		
		//Add it..
		mMiningTransactions.put(hash, zTrans);
		
		return true;
	}
	
	public void remeoveMiningTransaction(Transaction zTrans) {
		//Hash it..
		MiniData transhash = Crypto.getInstance().hashObject(zTrans, 160);
		String hash        = transhash.to0xString();
		mMiningTransactions.remove(hash);
	}
	
	public boolean checkInputForMining(MiniData zCoinID) {
		Enumeration<Transaction> alltrans = mMiningTransactions.elements();
		while(alltrans.hasMoreElements()) {
			Transaction trans = alltrans.nextElement();
			ArrayList<Coin> inputs = trans.getAllInputs();
			for(Coin input : inputs) {
				if(zCoinID.isEqual(input.getCoinID())) {
					return true;
				}
			}
		}
		
		return false;
	}
	
	
	public ArrayList<Coin> getTotalSimpleSpendableCoins(MiniData zTokenID) {
		ArrayList<Coin> confirmed   = new ArrayList<>();
		if(getMainTree().getChainRoot() == null) {
			return confirmed;
		}
		
		MiniNumber top = getTopBlock();
		
		//Check NONE of these are in the mempool.
		ArrayList<Coin> memcoins = getMempoolCoins();
				
		//Do we have any inputs with this address..
		ArrayList<CoinDBRow> relevant = getCoinDB().getCompleteRelevant();
		for(CoinDBRow row : relevant) {
			if(row.isInBlock() && !row.isSpent() && row.getCoin().getTokenID().isEqual(zTokenID)){
				MiniNumber depth = top.sub(row.getInBlockNumber());
				if(depth.isMoreEqual(GlobalParams.MINIMA_CONFIRM_DEPTH)) {
					//Is this a simple address..
					if(getUserDB().isSimpleAddress(row.getCoin().getAddress())) {
						boolean found   = false;
						MiniData coinid = row.getCoin().getCoinID();
						
						//Check in MemPool
						for(Coin memcoin : memcoins) {
							if(memcoin.getCoinID().isEqual(coinid)) {
								found = true;
								break;
							}
						}
						
						//Check in Mining
						if(!found) {
							found = checkInputForMining(coinid);	
						}
						
						//Is it safe to use ?
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
			TxPoW txpow = row.getTxPOW();
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
			TxPoW txpow = row.getTxPOW();
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
	
	public Hashtable<String, MiniNumber> getTransactionTokenAmounts(TxPoW zTxPOW) {
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
		
		//Which MMRSet to use.. use the same one for the whole transaction
		MiniNumber howdeep = currentblock.sub(recent);
		
		//MAX 256 blocks in the past 'should' be fine.. so re-orgs won't invalidate it..
		if(howdeep.isMore(GlobalParams.MINIMA_MMR_PROOF_HISTORY)) {
			howdeep = GlobalParams.MINIMA_MMR_PROOF_HISTORY;
		}
		
//		//DEBUG..
//		if(GlobalParams.SHORT_CHAIN_DEBUG_MODE) {
//			if(howdeep.isMore(MiniNumber.FOUR)) {
//				howdeep = MiniNumber.FOUR;
//			}
//		}
	
		//Check not past the cascade..
		MiniNumber proofblock = currentblock.sub(howdeep);
		if(proofblock.isLess(getMainTree().getCascadeNode().getBlockNumber())) {
			proofblock = getMainTree().getCascadeNode().getBlockNumber();
		}
		
		//The Actual MMR block we will use..
		MMRSet proofmmr = basemmr.getParentAtTime(proofblock);
		
		//Now add the actual MMR Proofs..
		for(Coin cc : ins) {
			MiniNumber entrynum = null;
			
			//Get the entry
			CoinDBRow crow = getCoinDB().getCoinRow(cc.getCoinID());
			if(crow != null) {
				entrynum = crow.getMMREntry();
			}else {
				entrynum = proofmmr.findEntry(cc.getCoinID()).getEntryNumber();
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
				zConfirmed, zTokenID, zChangeTokenID, zTokenGen, new Transaction(),"",true);
	}
		
	public Message createTransaction(MiniNumber zAmount, Address zToAddress, 
				 Address zChangeAddress, 
				 ArrayList<Coin> zConfirmed,
				 MiniData zTokenID,
				 MiniData zChangeTokenID,
				 TokenProof zTokenGen, 
				 Transaction zUseThisTransaction,
				 String zStateVars,
				 boolean zSignTransaction) {
		
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
		
		//State Variables..
		if(!zStateVars.equals("")) {
			StringTokenizer strtok = new StringTokenizer(zStateVars,"#");
			while(strtok.hasMoreElements()){
				String sttok = strtok.nextToken().trim();
				
				//Now split this token..
				if(!sttok.equals("")) {
					int split = sttok.indexOf(":");
					String statenum = sttok.substring(0,split).trim();
					String value    = sttok.substring(split+1).trim();
					
					//Set it..
					trx.addStateVariable(new StateVariable(Integer.parseInt(statenum), value));
				}
			}
		}
		
		
		//Now we have a full transaction we can sign it!
		if(zSignTransaction) {
			MiniData transhash = Crypto.getInstance().hashObject(trx);
			for(MiniData pubk : sigpubk) {
				//Get the Pub Priv..
				MultiKey signer = getUserDB().getPubPrivKey(pubk);
				
				//Sign the data
				MiniData signature = signer.sign(transhash);
				
				//Add to the witness..
				wit.addSignature(pubk, signature);	
			}
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
	public TxPoW getCurrentTxPow(Transaction zTrans, Witness zWitness, JSONArray zContractLogs) {
		//Get the current best block..
		BlockTreeNode tip = mMainTree.getChainTip();
		
		if(tip == null) {
			MinimaLogger.log("getCurrentTxPow - NO BLOCKS NULL TIP!");
			return null;
		}
		
		//TODO - Add Burn Transaction and Witness!
		//..
		
		//Parent TxPOW
		TxPoW parenttxpow = tip.getTxPow();
				
		//Fresh TxPOW
		TxPoW txpow = new TxPoW();
				
		//Set the time
		txpow.setTimeMilli(new MiniNumber(""+System.currentTimeMillis()));
			
		//Set the Transaction..
		txpow.setTransaction(zTrans);
		txpow.setWitness(zWitness);
		
		//Block and Transaction difficulty
		txpow.setTxDifficulty(TxPoWMiner.BASE_TXN);
		
		//Are we debugging
		if(GlobalParams.MINIMA_ZERO_DIFF_BLK) {
			//Minimum Difficulty - any hash will do
			txpow.setBlockDifficulty(TxPoWMiner.BASE_BLOCK);	
		}else {
			txpow.setBlockDifficulty(TxPoWMiner.BASE_TXN);		
		}
		
		//Block Details..
		MiniNumber currenttip = tip.getTxPow().getBlockNumber();
		txpow.setBlockNumber(currenttip.increment());
		
		//Do we have enough blocks to get an accurate speed reading..
		if(!GlobalParams.MINIMA_ZERO_DIFF_BLK && currenttip.isMore(GlobalParams.MINIMA_BLOCKS_SPEED_CALC) ) {
			//Desired Speed.. in blocks per second
			MiniNumber actualspeed 	= mMainTree.getChainSpeed(tip);
			
			//Calculate the speed ratio
			MiniNumber speedratio   = GlobalParams.MINIMA_BLOCK_SPEED.div(actualspeed);
			
//			//Check within acceptable parameters..
//			MiniNumber high = MiniNumber.ONE.add(GlobalParams.MINIMA_MAX_SPEED_RATIO);
//			MiniNumber low  = MiniNumber.ONE.sub(GlobalParams.MINIMA_MAX_SPEED_RATIO);
//			if(speedratio.isMore(high)){
//				//MinimaLogger.log("SPEED RATIO TOO HIGH : "+speedratio);
//				speedratio = high;
//			}else if(speedratio.isLess(low)){
//				//MinimaLogger.log("SPEED RATIO TOO LOW : "+speedratio);
//				speedratio = low;
//			}
			
			//Current average
			BigInteger avgdiff = mMainTree.getAvgChainDifficulty(tip);
			BigDecimal avgdiffdec = new BigDecimal(avgdiff);
			
			//Multiply by the ratio
			BigDecimal newdiffdec = avgdiffdec.multiply(speedratio.getAsBigDecimal());
			BigInteger newdiff    = newdiffdec.toBigInteger();
						
			//Check more than TX-MIN..
			if(newdiff.compareTo(Crypto.MEGA_VAL)>0) {
				newdiff = Crypto.MEGA_VAL;
			}
							
			//Create the hash
			MiniData diffhash = new MiniData("0x"+newdiff.toString(16)); 
			
			//Now set the new difficulty
			txpow.setBlockDifficulty(diffhash);
		}
		
		//Super Block Levels.. FIRST just copy them all..
		for(int i=0;i<GlobalParams.MINIMA_CASCADE_LEVELS;i++) {
			txpow.setSuperParent(i, tip.getTxPow().getSuperParent(i));
		}

		//And now set the correct SBL given the last block
		int sbl = tip.getSuperBlockLevel();
		
		//All levels below this now point to the last block..
		MiniData tiptxid = tip.getTxPowID();
		for(int i=sbl;i>=0;i--) {
			txpow.setSuperParent(i, tiptxid);
		}			
		
		//Get the current MMRSet
		MMRSet newset = new MMRSet(tip.getMMRSet());
		
		//Check the first transaction
		MiniNumber txncounter = MiniNumber.ZERO;
		if(!zTrans.isEmpty()) {
			boolean valid = TxPoWChecker.checkTransactionMMR(zTrans, zWitness, this, txpow, newset, true, zContractLogs);
			
			//MUST be valid.. ?
			if(!valid) {
				MinimaLogger.log("ERROR: Your own transaction is invalid !?");
				return null;
			}
		}
		
		//Set the current Transaction List!
		ArrayList<TxPOWDBRow> unused = mTxPOWDB.getAllUnusedTxPOW();
		for(TxPOWDBRow row : unused) {
			//Current MAX transactions.. #TODO.. this needs to be dynamic..
			if(txncounter.isMore(MiniNumber.SIXTYFOUR)) {
				break;
			}
			
			//Check is still VALID..
			TxPoW txp = row.getTxPOW();
			
			/**
			 * MUST be a transaction as that prevents double entry. A block with no transaction 
			 * is valid.. but no way to check it has already been added
			 */
			if(txp.isTransaction()) {
				MiniNumber txncountertest = txncounter.increment();
				
				//First check it without touching the MMR
				
				boolean valid = TxPoWChecker.checkTransactionMMR(txp, this, txpow, newset,true);
				
				
				
				if(valid) {
					//Valid so added
					txncounter = txncountertest;
					
					//Add it..
					txpow.addBlockTxPOW(txp);	
				
				}else {
					//Could be a transaction that is only valid in a different  branch.					
					MinimaLogger.log("Invalid TXPOW found. (leaving.. could be in other branch) "+txp.getTxPowID());
				}
			}else {
				//ONLY ADD VALID TRANSACTIONS - the mmr is the checker for previous inclusion
				//and a non-transaction has no mmr data to chcek
//				//A block with no transaction.. make sure within range..
//				if(!txp.getBlockNumber().sub(txpow.getBlockNumber()).abs().isMoreEqual(MiniNumber.EIGHT)) {
//					//Valid so added
//					txncounter = txncounter.increment();
//						
//					//Add it..
//					txpow.addBlockTxPOW(txp);		
//				}
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
	
	public TxPoW getTopTxPoW() {
		return getMainTree().getChainTip().getTxPow();
	}
	
	public SyncPackage getSyncPackage() {
		SyncPackage sp = new SyncPackage();
		
		//Is there anything.. ?
		if(getMainTree().getChainRoot()==null) {
			return sp;
		}
		
		//Cascade Node
		MiniNumber casc = getMainTree().getCascadeNode().getTxPow().getBlockNumber();
		sp.setCascadeNode(casc);
		
		//Lets create a sync package
		ArrayList<BlockTreeNode> nodes = getMainTree().getAsList();
		
		//Cycle through it all..
		for(BlockTreeNode node : nodes) {
			MiniNumber block = node.getTxPow().getBlockNumber();
			sp.getAllNodes().add(0,new SyncPacket(node, block.isLess(casc)));
		}
		
		//Now create a DEEP copy
		SyncPackage spdeep = new SyncPackage();
		try {
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			DataOutputStream dos = new DataOutputStream(baos);
			sp.writeDataStream(dos);
			dos.flush();
			
			//And read it in..
			ByteArrayInputStream bais = new ByteArrayInputStream(baos.toByteArray());
			DataInputStream dis = new DataInputStream(bais);
			
			//Now read it in.. 
			spdeep.readDataStream(dis);
			
			//Clean up
			dos.close();
			baos.close();
			
			dis.close();
			bais.close();
			
		}catch(Exception exc) {
			exc.printStackTrace();
		}	
		
//		//Now remove all the bodies..
//		ArrayList<SyncPacket> packs = spdeep.getAllNodes();
//		for(SyncPacket pack : packs) {
//			pack.getTxPOW().clearBody();
//		}
	
		//Clean up after this large-ish event..
		System.gc();
		
		//Return the lite sync package
		return spdeep;
	}
		
	/**
	 * Get the Current IBD - total required to log in for a new user
	 */
	public int getIntroSyncSize() {
		SyncPackage sp = getSyncPackage();
		
		//Write it out..
		try {
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			DataOutputStream dos = new DataOutputStream(baos);
			sp.writeDataStream(dos);
			dos.flush();
			
			int len = baos.toByteArray().length;
			
			dos.close();
			baos.close();
			
			return len;
			
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
