package org.minima.database.txpowtree;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Hashtable;
import java.util.Random;

import org.minima.GlobalParams;
import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMRSet;
import org.minima.database.txpowdb.TxPOWDBRow;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;

public class BlockTree {
	
	/**
	 * ROOT node of the Chain
	 */
	private BlockTreeNode mRoot;
	
	/**
	 * The Tip of the longest Chain
	 */
	private BlockTreeNode mTip;
	
	/**
	 * The Block beyond which you are cascading and parents may be of a higher level
	 */
	private BlockTreeNode mCascadeNode;
	
	/**
	 * The FAST link from ID to Block..
	 */
	private Hashtable<String, BlockTreeNode> mFastLink;
	
	/**
	 * This is a list of all the VALID blocks - used to cascade the chain
	 */
	private ArrayList<BlockTreeNode> mValidBlockList;
	
	/**
	 * Main Constructor
	 */
	public BlockTree() {
		clearTree();
	}
	
	public void setTreeRoot(BlockTreeNode zNode) {
		zNode.setParent(null);
		mRoot 			= zNode;
		mTip 			= mRoot;
		mCascadeNode 	= mRoot;
		
		//The list of Nodes..
		mValidBlockList = new ArrayList<>();
		
		//New root - new tree - reset fasttable
		mFastLink = new Hashtable<>();
		
		//Add to the Fast List..
		addFastLinkNode(zNode);
	}
	
	public BlockTreeNode getChainRoot() {
		return mRoot;
	}
	
	public BlockTreeNode getChainTip() {
		return mTip;
	}
	
	public BlockTreeNode getCascadeNode() {
		return mCascadeNode;
	}
	
	/**
	 * The parent MUST exist before calling this!
	 * 
	 * @param zNode
	 * @return
	 */
	public boolean addNode(BlockTreeNode zNode) {
		return addNode(zNode, false);
	}
	
	public boolean addNode(BlockTreeNode zNode, boolean zFromBackup) {
		//Do we have it allready.. 
		BlockTreeNode exists = findNode(zNode.getTxPowID());
		if(exists != null) {
			//All ready in there..
			return false;
		}
		
		//Find the parent block.. from last uncascaded node onwards
		BlockTreeNode parent = findNode(zNode.getTxPow().getParentID());
		
		//Do we have a parent..
		if(parent == null) {
			return false;
		}
		
		/**
		 * You need this so that the average speed and difficulty can be worked out..
		 */
		if(mTip.getBlockNumber().isMore(GlobalParams.MINIMA_BLOCKS_SPEED_CALC)) {
			MiniNumber minblock = getCascadeNode().getBlockNumber().add(GlobalParams.MINIMA_BLOCKS_SPEED_CALC);
			if(zNode.getBlockNumber().isLessEqual(minblock)) {
				if(!zFromBackup) {
					MinimaLogger.log("BlockTree : BLOCK PAST MIN ALLOWED NODE ["+minblock+"].. "+zNode.getTxPow().getBlockNumber()+" "+zNode.getTxPow().getTxPowID());
					return false;
				}
			}
		}
		
		//It's OK - add it
		parent.addChild(zNode);

		//Add to the fast list
		addFastLinkNode(zNode);
		
		//It's been added
		return true;
	}
	
	/**
	 * Adds the node regardless of the parents.. used when cascading
	 * @param zNode
	 * @return
	 */
	public void hardAddNode(BlockTreeNode zNode, boolean zTouchMMR) {
		if(mRoot == null) {
			setTreeRoot(zNode);
			return;
		}
		
		//Add to the end..
		mTip.addChild(zNode);
		
		//Link the MMRSet
		if(zTouchMMR) {
			if(zNode.getMMRSet() != null) {
				if(mTip.getTxPowID().isEqual(zNode.getTxPow().getParentID())) {
					//Correct Parent.. can link the MMR!
					zNode.getMMRSet().setParent(mTip.getMMRSet());	
				}else {
					zNode.getMMRSet().setParent(null);
				}
			}
		}
				
		//Move on..
		mTip = zNode;
		
		//Add to the fast list
		addFastLinkNode(zNode);
	}
	
	public void hardSetCascadeNode(BlockTreeNode zNode) {
		mCascadeNode = zNode;
	}
	
	/**
	 * Resets the weights in the tree
	 */
	public void resetWeights() {
		//First default them
		zeroWeights();
	
		//Start at root..
		_cascadeWeights();
		
		//And get the tip..
		mTip = _getHeaviestBranchTip();		
	}
	
	/**
	 * Set all the weights to 0 - and remake the fastlink table
	 */
	public void zeroWeights() {
		//Clear the current table..
		mFastLink.clear();
		mValidBlockList.clear();	
		
		//Go down the whole tree..
		_recurseTree(new NodeAction() {
			@Override
			public void runAction(BlockTreeNode zNode) {
				//Reset the weight..
				zNode.resetCurrentWeight();
				
				//Add to the fast list
				addFastLinkNode(zNode);
				
				//Is this a valid node..
				if(zNode.getState() == BlockTreeNode.BLOCKSTATE_VALID) {
					mValidBlockList.add(zNode);
				}
			}
		});
	}
	
	private void addFastLinkNode(BlockTreeNode zNode) {
		//Add to the HashTable
		String id = zNode.getTxPowID().to0xString();
		
		//Add to the Table..
		mFastLink.put(id, zNode);
	}
	
	/**
	 * Calculate the correct weights per block on the chain using GHOST
	 */
	private void _cascadeWeights() {
		//Order the list in the array.. from top to bottom..
		mValidBlockList.sort(new Comparator<BlockTreeNode>() {
			@Override
			public int compare(BlockTreeNode o1, BlockTreeNode o2) {
				return o2.getBlockNumber().compareTo(o1.getBlockNumber());
			}
		});
		
		//Now add the weight of children to the parent.. 
		for(BlockTreeNode node : mValidBlockList) {
			//Get the children
			ArrayList<BlockTreeNode> children = node.getChildren();
			for(BlockTreeNode child : children) {
				if(child.getState() == BlockTreeNode.BLOCKSTATE_VALID) {
					node.addToTotalWeight(child.getTotalWeight());
				}
			}
		}
	}
	
	/**
	 * Pick the GHOST heaviest Branch of the tree
	 * @param zStartNode
	 * @return
	 */
	private BlockTreeNode _getHeaviestBranchTip() {
		//Hmm.. Should start at cascade node.. MUST be past that node anyway..
		BlockTreeNode curr = getCascadeNode();
		
		//If null return null
		while(curr != null ) {
			//Get the heaviest child branch
			ArrayList<BlockTreeNode> children = curr.getChildren();
			
			//Do we have any children
			if(children.size()==0) {
				return curr;
			}
			
			//Only keep the heaviest
			BlockTreeNode heavy = null;
			for(BlockTreeNode node : children) {
				if(node.getState() == BlockTreeNode.BLOCKSTATE_VALID) {
					if(heavy == null) {
						heavy = node;
					}else {
						if(node.getTotalWeight().compareTo(heavy.getTotalWeight()) > 0) {
							heavy = node;
						}
					}
				}
			}
			
			//No valid children..
			if(heavy == null){
				return curr;
			}
			
			//reset and do it again!
			curr = heavy;
		}
		
		return curr;
	}
	
	/**
	 * Find a specific node in the tree
	 * 
	 * @param zTxPOWID
	 * @return
	 */
	
	public BlockTreeNode findNode(MiniData zTxPOWID) {
		return findNode(zTxPOWID, false);
	}
	
	/**
	 * Double Drill - after a cascade the fast link table not set up..
	 */
	public BlockTreeNode findNode(MiniData zTxPOWID, boolean zRecurseAlso) {
		//Check fast link table
		BlockTreeNode fastnodefind = mFastLink.get(zTxPOWID.to0xString());
		if(fastnodefind!=null) {
			return fastnodefind;
		}
		
		//Are we recursing if we can;t find it - cascade tree needs this..
		if(zRecurseAlso) {
			//MinimaLogger.log("BLOCKTREE TIP NOT FOUND : Recurse required.. "+zTxPOWID);
			
			//SLOWER recursive method.. replaced by the fast hashtable
			NodeAction finder = new NodeAction(zTxPOWID) {
				@Override
				public void runAction(BlockTreeNode zNode) {
					if(zNode.getTxPowID().isEqual(getExtraData())) {
	        			setReturnObject(zNode);
	        		}
				}
			}; 
			
			return _recurseTree(finder);	
		}
		
		//Not found..
		return null;
	}
	
	/**
	 * Find the media time of the last N blocks..
	 */
	private MiniNumber getMedianTime(BlockTreeNode zNode, int zLastBlocks) {
		//get the current..
		BlockTreeNode current = zNode;
		
		//First make an array of all the numbers..
		ArrayList<MiniNumber> timelist = new ArrayList<>();
		
		int check =0;
		while(check++ < zLastBlocks) {
			timelist.add(current.getTxPow().getTimeSecs());
			
			current = current.getParent();
			if(current == null) {
				break;
			}
		}
		
		//Now sort them..
		Collections.sort(timelist, new Comparator<MiniNumber>() {
			@Override
			public int compare(MiniNumber o1, MiniNumber o2) {
				if(o1.isLess(o2)) {
					return 1;
				}else if(o2.isLess(o1)) {
					return -1;
				}
				return 0;
			}
		});
		
		return timelist.get(check / 2);
	}
	
	
	/**
	 * Sort the Block tree nodes.. ONLY Full blocks with valid parents get checked
	 * @param zMainDB
	 */
	public void sortBlockTreeNodeStates(MinimaDB zMainDB) {
		//Action that checks for a specific node..
		NodeAction nodestates = new NodeAction(zMainDB) {
			@Override
			public void runAction(BlockTreeNode zNode) {
				//Default state
				int parentstate = BlockTreeNode.BLOCKSTATE_INVALID;
				
				//Check for chain root..
				if(getChainRoot().getTxPowID().isEqual(zNode.getTxPowID())) {
					parentstate = BlockTreeNode.BLOCKSTATE_VALID;
				}else{
					parentstate = zNode.getParent().getState();
				}
				
				//Must be a valid parent for anything to happen
				if(parentstate == BlockTreeNode.BLOCKSTATE_INVALID) {
					//All Children are INVALID
					zNode.setState(BlockTreeNode.BLOCKSTATE_INVALID);
				
				}else if(parentstate == BlockTreeNode.BLOCKSTATE_VALID) {
					//Only check if unchecked..
					if(zNode.getState() == BlockTreeNode.BLOCKSTATE_BASIC) {
						//Get the txpow row - do this now as slow function
						TxPOWDBRow row = getDB().getTxPOWRow(zNode.getTxPowID());
						
						//Is it full
						if(row.getBlockState() == TxPOWDBRow.TXPOWDBROW_STATE_FULL) {
							//Need all ok for the block to be accepted
							boolean allok = false;
							
							//The Parent block..
							BlockTreeNode pnode = zNode.getParent();
							
							//Does it have a valid MMR.. or is it too late for this block.. the parent is a cascade node.
							if(pnode.getMMRSet() == null) {
								//MinimaLogger.log("NULL PARENT MMR "+zNode.getBlockNumber()+" "+getCascadeNode().getBlockNumber());
								zNode.setState(BlockTreeNode.BLOCKSTATE_INVALID);
								return;
							}
							
							//Check block number..
							if(!zNode.getBlockNumber().isEqual(pnode.getBlockNumber().increment())){
								MinimaLogger.log("INVALID BLOCK NUMBER for Parent "+zNode.getBlockNumber());
								zNode.setState(BlockTreeNode.BLOCKSTATE_INVALID);
								return;
							}
							
							//Check that the TIME is within acceptable parameters - 30 minutes each way
							MiniNumber mediantime = getMedianTime(zNode, 72);
							MiniNumber nodetime   = zNode.getTxPow().getTimeMilli();
							MiniNumber timefuture = new MiniNumber(System.currentTimeMillis()).add(new MiniNumber(30 * 60 * 1000));   
							if(nodetime.isLess(mediantime) || nodetime.isMore(timefuture)) {
								//Time is incorrect!.. must be greater than the median of the last 100 blocks..
								MinimaLogger.log("INVALID BLOCK TIME "+zNode.getBlockNumber()+") med:"+mediantime+" / node:"+nodetime+ " / fut:"+timefuture);
								zNode.setState(BlockTreeNode.BLOCKSTATE_INVALID);
								return;
							}
							
							//Whats the minimum block for speed calculation..
							MiniNumber speedblock = getDB().getMainTree().getCascadeNode().getBlockNumber().add(GlobalParams.MINIMA_BLOCKS_SPEED_CALC);
							if(!GlobalParams.MINIMA_ZERO_DIFF_BLK && zNode.getBlockNumber().isMore(speedblock)) {
								
								//Check that Block difficulty is Correct
								MiniNumber actualspeed 	= getDB().getMainTree().getChainSpeed(pnode);
								MiniNumber speedratio   = GlobalParams.MINIMA_BLOCK_SPEED.div(actualspeed);
								
	//							//Check within acceptable parameters..
	//							MiniNumber high = MiniNumber.ONE.add(GlobalParams.MINIMA_MAX_SPEED_RATIO);
	//							MiniNumber low  = MiniNumber.ONE.sub(GlobalParams.MINIMA_MAX_SPEED_RATIO);
	//							if(speedratio.isMore(high)){
	//								speedratio = high;
	//							}else if(speedratio.isLess(low)){
	//								speedratio = low;
	//							}
								
								//Current average
								BigInteger avgdiff    = getDB().getMainTree().getAvgChainDifficulty(pnode);
								BigDecimal avgdiffdec = new BigDecimal(avgdiff);
								
								//Multiply by the ratio
								BigDecimal newdiffdec = avgdiffdec.multiply(speedratio.getAsBigDecimal());
								BigInteger newdiff    = newdiffdec.toBigInteger();
											
								//Check more than TX-MIN..
								if(newdiff.compareTo(Crypto.MEGA_VAL)>0) {
									newdiff = Crypto.MEGA_VAL;
								}
								MiniData diffhash = new MiniData("0x"+newdiff.toString(16)); 
									
								if(!zNode.getTxPow().getBlockDifficulty().isEqual(diffhash)) {
									MinimaLogger.log("INVALID BLOCK DIFFICULTY "+zNode.getBlockNumber());
									zNode.setState(BlockTreeNode.BLOCKSTATE_INVALID);
									return;
								}
							}
							
							//Check the Super Block Levels are Correct! and point to the correct blocks
							//..TODO
							
							//need a  body for this..
							if(row.getTxPOW().hasBody()) {
								//Create an MMR set that will ONLY be used if the block is VALID..
								MMRSet mmrset = new MMRSet(pnode.getMMRSet());
								
								//Set this MMR..
								zNode.setMMRset(mmrset);
								
								//Check all the transactions in the block are correct..
								allok = getDB().checkAllTxPOW(zNode, mmrset);
								
								//Check the root MMR..
								if(allok) {
									if(!row.getTxPOW().getMMRRoot().isEqual(mmrset.getMMRRoot().getFinalHash())) {
										MinimaLogger.log("INVALID BLOCK MMRROOT "+zNode.getBlockNumber());
										allok = false;	
									}
									
									if(!row.getTxPOW().getMMRTotal().isEqual(mmrset.getMMRRoot().getValueSum())) {
										MinimaLogger.log("INVALID BLOCK MMRSUM "+zNode.getBlockNumber());
										allok = false;
									}
								}else {
									MinimaLogger.log("INVALID BLOCK TRANSACTIONS "+zNode.getBlockNumber());
								}
							}else {
								MinimaLogger.log("INVALID BLOCK no body TxPoW..! "+zNode.toString());
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
				}
			}
		}; 
		
		_recurseTree(nodestates);
	}
	
	/**
	 * Simple Printer
	 */
	public void printTree() {
		//Action that checks for a specific node..
		NodeAction printer = new NodeAction() {
			@Override
			public void runAction(BlockTreeNode zNode) {
				BlockTreeNode parent = zNode.getParent();
				if(parent!=null) {
					MinimaLogger.log(zNode.getTxPowID().to0xString(10)+" parent:"+zNode.getParent().getTxPowID());	
				}else {
					MinimaLogger.log(zNode.getTxPowID().to0xString(10)+" ROOT");
				}
			}
		}; 
		
		_recurseTree(printer);
	}
	
	private BlockTreeNode _recurseTree(NodeAction zNodeAction) {
		//If nothing on chain return nothing
		if(getChainRoot() == null) {return null;}
				
		//Create a STACK..
		NodeStack stack = new NodeStack();
		
		//Push the root on the stack
		stack.push(getChainRoot());
		
		//Now cycle..
		while(!stack.isEmpty()) {
			//Get the top stack item
			BlockTreeNode node = stack.pop();
			
			//Do the action..
			zNodeAction.runAction(node);
    		
    		//Have we found what we were looking for..
    		if(zNodeAction.returnObject()) {
    			return zNodeAction.getObject();
    		}
    		
    		//Get any children..
    		ArrayList<BlockTreeNode> children = node.getChildren();
			for(BlockTreeNode child : children) {
				//Push on the stack
				stack.push(child);	
			}
		}
		
        return null;
	}
	
	/**
	 * Deep copy a node and it's children..
	 * 
	 * @param zStartNode
	 * @return
	 */
	public static BlockTreeNode copyTreeNode(BlockTreeNode zStartNode) {
		//Create a STACK..
		NodeStack stack     = new NodeStack();
		NodeStack copystack = new NodeStack();
		
		//The copy of the root node..
		BlockTreeNode rootCopy = new BlockTreeNode(zStartNode);
				
		//Push on the stack
		stack.push(zStartNode);
		copystack.push(rootCopy);
		
		//Now cycle..
		while(!stack.isEmpty()) {
			//Get the top stack item
			BlockTreeNode node     = stack.pop();
			BlockTreeNode copynode = copystack.pop();
			
    		//Get any children..
    		ArrayList<BlockTreeNode> children = node.getChildren();
			for(BlockTreeNode child : children) {
				//Create a copy..
				BlockTreeNode copychild = new BlockTreeNode(child);
				
				//Add to the copy..
				copynode.addChild(copychild);
			
				//Push on the stack
				stack.push(child);	
				copystack.push(copychild);
			}
		}
		
        return rootCopy;
	}
	
	/**
	 * Get the list of TreeNodes in the LongestChain
	 */
	public ArrayList<BlockTreeNode> getAsList(){
		return getAsList(false);
	}
	
	public ArrayList<BlockTreeNode> getAsList(boolean zReverse){
		ArrayList<BlockTreeNode> nodes  = new ArrayList<>();
	
		//Do we have a tip.. ?
		if(mTip == null) {
			return nodes;
		}
		
		//Add to the list..
		nodes.add(mTip);
		
		//Cycle up through the parents
		BlockTreeNode tip = mTip.getParent();
		while(tip != null) {
			if(zReverse) {
				nodes.add(0,tip);
			}else {
				nodes.add(tip);	
			}
			tip = tip.getParent();
		}
				
		return nodes;
	}
	
	/** 
	 * Clear the TxPoW Body and MMRset from all nodes past the cascade
	 */
	public void clearCascadeBody() {
		if(mCascadeNode == null) {
			return;
		}
		
		//Set the MMRSet parent to NULL
		mCascadeNode.getMMRSet().setParent(null);	
		if(mCascadeNode.getMMRSet().getMMRPeaks().size() == 0) {
			MinimaLogger.log("0 PEAKS! at cascade "+mCascadeNode.getBlockNumber());
		}
	
		//Clear from one node up..
		BlockTreeNode clearnode = mCascadeNode.getParent();
		while(clearnode != null) {
			//Clear the TxPoW
			clearnode.getTxPow().clearBody();
			
			//Clear the MMRset
			clearnode.setMMRset(null);
		
			//Get the Parent
			clearnode = clearnode.getParent();
		}
	}
	
	/**
	 * Get a past block..
	 * 
	 * @param zNumberFromTip
	 * @return the blocktreenode
	 */
	public BlockTreeNode getPastBlock(BlockTreeNode zStartPoint, int zNumberFromTip) {
		MiniNumber cascnumber = mCascadeNode.getTxPow().getBlockNumber();
		BlockTreeNode current = zStartPoint;
		int tot               = 0;
		while(current.getBlockNumber().isMore(cascnumber) && tot<zNumberFromTip) {
			BlockTreeNode parent = current.getParent();
			if(parent != null) {
				current = parent;
				tot++;
			}else {
				break;
			}
		}
		
		return current;
	}
	
	
	/**
	 * Get the Chain Speed..
	 * 
	 * Calculated as the different between the cascade node and the tip..
	 */
	public MiniNumber getChainSpeed() {
		return getChainSpeed(mTip);
	}
	
	public MiniNumber getChainSpeed(BlockTreeNode zStartPoint) {
		//Use a previous block.. 
		BlockTreeNode starter = getPastBlock(zStartPoint, GlobalParams.MINIMA_BLOCKS_SPEED_CALC.getAsInt());
		
		//Calculate to seconds..
		MiniNumber start      = starter.getTxPow().getTimeSecs();
		MiniNumber end        = zStartPoint.getTxPow().getTimeSecs();
		MiniNumber timediff   = end.sub(start);
		
		//How many blocks..
		MiniNumber blockstart = starter.getTxPow().getBlockNumber();
		MiniNumber blockend   = zStartPoint.getTxPow().getBlockNumber();
		MiniNumber blockdiff  = blockend.sub(blockstart); 
		
		//So.. 
		if(timediff.isEqual(MiniNumber.ZERO)) {
			return MiniNumber.ONE;
		}
		MiniNumber speed  = blockdiff.div(timediff);
		
		return speed;
	}
	
	/**
	 * Get the current average difficulty
	 */
	public BigInteger getAvgChainDifficulty() {
		return getAvgChainDifficulty(mTip);	
	}
	
	public BigInteger getAvgChainDifficulty(BlockTreeNode zStartPoint) {
		//The Total..
		BigInteger totaldifficulty = new BigInteger("0");
		int numberofblocks=0;
		
		//Cycle back from the tip..
		BlockTreeNode starter   = getPastBlock(zStartPoint, GlobalParams.MINIMA_BLOCKS_SPEED_CALC.getAsInt());
		MiniNumber minblock     = starter.getTxPow().getBlockNumber();
		BlockTreeNode current 	= zStartPoint;
				
		while(current!=null && current.getBlockNumber().isMoreEqual(minblock)) {
			//Add to the total
			totaldifficulty = totaldifficulty.add(current.getTxPow().getBlockDifficulty().getDataValue());
			numberofblocks++;
			
			//Get the parent
			current = current.getParent();
		}
		
		//The Average
		BigInteger avg = totaldifficulty.divide(new BigInteger(""+numberofblocks));
		
		return avg;
	}

	public void clearTree() {
		mRoot 			= null;
		mTip 			= null;
		mCascadeNode 	= null;
		mFastLink       = new Hashtable<>();
		mValidBlockList = new ArrayList<>();
	}

	public static TxPoW createRandomTxPow() {
		TxPoW txpow = new TxPoW();
		txpow.setHeaderBodyHash();
		txpow.calculateTXPOWID();
		return txpow;
	}
	
	public static void main(String[] zArgs) {
		
		
		//Test the cascade..
		ArrayList<Integer> thelist = new ArrayList<>();
		Random rand = new Random();
		
		for(int i=0;i<32000;i++) {
			thelist.add(new Integer(rand.nextInt()));
		}
		
		
		long timenow = System.currentTimeMillis();
		
		//Order the state
		Collections.sort(thelist,new Comparator<Integer>() {
			@Override
			public int compare(Integer o1, Integer o2) {
				return Integer.compare(o1,o2);
			}
		});
		
		long timediff = System.currentTimeMillis() - timenow;
		
		System.out.println("Total time : "+timediff);
		
		
		
		
		
//		BlockTree tree = new BlockTree();
//		
//		TxPoW root = createRandomTxPow();
//		BlockTreeNode rootnode = new BlockTreeNode(root);
//		tree.setTreeRoot(rootnode);
//		System.out.println("root : "+rootnode.getTxPowID().to0xString(10));
//		
//		//2 kids..
//		TxPoW child = createRandomTxPow();
//		BlockTreeNode treenode = new BlockTreeNode(child);
//		rootnode.addChild(treenode);
//		System.out.println("rootchild1 : "+treenode.getTxPowID().to0xString(10));
//		
//		TxPoW child4 = createRandomTxPow();
//		BlockTreeNode treenode4 = new BlockTreeNode(child4);
//		treenode.addChild(treenode4);
//		System.out.println("child1child1 : "+treenode4.getTxPowID().to0xString(10));
//		
//		TxPoW child5 = createRandomTxPow();
//		BlockTreeNode treenode5 = new BlockTreeNode(child5);
//		treenode.addChild(treenode5);
//		System.out.println("child1child2 : "+treenode5.getTxPowID().to0xString(10));
//		
//		TxPoW child6 = createRandomTxPow();
//		BlockTreeNode treenode6 = new BlockTreeNode(child6);
//		treenode.addChild(treenode6);
//		System.out.println("child1child3 : "+treenode6.getTxPowID().to0xString(10));
//		
//		TxPoW child2 = createRandomTxPow();
//		BlockTreeNode treenode2 = new BlockTreeNode(child2);
//		rootnode.addChild(treenode2);
//		System.out.println("rootchild2 : "+treenode2.getTxPowID().to0xString(10));
//		
//		TxPoW child3 = createRandomTxPow();
//		BlockTreeNode treenode3 = new BlockTreeNode(child3);
//		treenode2.addChild(treenode3);
//		System.out.println("child2child1 : "+treenode3.getTxPowID().to0xString(10));
//		
//		tree.printTree();
//		System.out.println();
//		
//		//Lets copy..
//		BlockTreeNode copy = BlockTree.copyTreeNode(rootnode);
//		BlockTree copytree = new BlockTree();
//		copytree.setTreeRoot(copy);
//	
//		copytree.printTree();
		
		
		//Search for the child..
//		System.out.println("\nSearch for "+child3.getTxPowID().to0xString(10)+"\n\n");
//		BlockTreeNode find =  tree.findNode(child3.getTxPowID());
//		BlockTreeNode find =  tree.findNode(MiniData.getRandomData(5));
		
		
	}
	
	
}
