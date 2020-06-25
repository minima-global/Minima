package org.minima.database.txpowtree;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Hashtable;

import org.minima.GlobalParams;
import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMRSet;
import org.minima.database.txpowdb.TxPOWDBRow;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.MinimaLogger;

public class BlockTree {
	
	/**
	 * ROOT node of the Chain
	 */
	BlockTreeNode mRoot;
	
	/**
	 * The Tip of the longest Chain
	 */
	public BlockTreeNode mTip;
	
	/**
	 * The Block beyond which you are cascading and parents may be of a higher level
	 */
	BlockTreeNode mCascadeNode;
	
	/**
	 * When searching for the tip.. 
	 */
	BlockTreeNode _mOldTip;
	
	/**
	 * When Copying..
	 */
	BlockTreeNode mCopyNode;
	
	/**
	 * The FAST link from ID to Block..
	 */
	Hashtable<String, BlockTreeNode> mFastLink;
	
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
		//Do we have it allready.. DOUBLE check as this done already
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

		//Can't add to less than
		MiniNumber minblock = getCascadeNode().getBlockNumber().add(GlobalParams.MINIMA_BLOCKS_SPEED_CALC);
		
		//Check after the cascade node.. - need a minimum first
		if(mTip.getBlockNumber().isMore(GlobalParams.MINIMA_BLOCKS_SPEED_CALC)) {
			if(zNode.getBlockNumber().isLessEqual(minblock)) {
				MinimaLogger.log("BlockTree : BLOCK PAST LAST ALLOWED NODE.. "+zNode.getTxPow());
				return false;
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
		//Store the Old Tip
		_mOldTip = mTip;
		
		//First default them
		_zeroWeights();
		
		//Start at root..
		_cascadeWeights();
		
		//And get the tip..
		mTip = _getHeaviestBranchTip();		
	}
	
	/**
	 * Set all the weights to 0
	 */
	private void _zeroWeights() {
		//Clear the current table..
		mFastLink = new Hashtable<>();
				
		//Go down the whole tree..
		_recurseTree(new NodeAction() {
			@Override
			public void runAction(BlockTreeNode zNode) {
				//Reset the weight..
				zNode.resetCurrentWeight();
				
				//Add to the fast list
				addFastLinkNode(zNode);
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
	 * Calculate the correct weights per block on the chain
	 */
	private void _cascadeWeights() {
		//First lets stream up the OLD main chain.. OPTIMISATION
		if(_mOldTip != null) {
			BigInteger weight        = _mOldTip.getWeight();
			_mOldTip.mCascadeWeighted = true;
			
			//Add to all the parents..
			BlockTreeNode parent = _mOldTip.getParent();
			while(parent != null) {
				//A new cascading weight
				BigInteger newweight = weight.add(parent.getWeight()); 
				
				//Add to this parent..
				parent.mCascadeWeighted = true;
				parent.addToTotalWeight(weight);
				parent = parent.getParent();
				
				//Set the new weight
				weight = newweight;
			}
		}
		
		//Add all the weights up..
		_recurseTree(new NodeAction() {
			@Override
			public void runAction(BlockTreeNode zNode) {
				//Only add valid blocks
				if(zNode.getState() == BlockTreeNode.BLOCKSTATE_VALID && !zNode.mCascadeWeighted) {
					//The weight of this block
					BigInteger weight = zNode.getWeight();
					
					//Add to all the parents..
					BlockTreeNode parent = zNode.getParent();
					while(parent != null) {
						parent.addToTotalWeight(weight);
						parent = parent.getParent();
					}
				}
			}
		});
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
							//Need allok for the block to be accepted
							boolean allok = false;
							
							//Check that Block difficulty is Correct!?
							//..TODO
							
							//Check the Super Block Levels are Correct! and point to the correct blocks
							//..TODO
							
							//need a  body for this..
							if(row.getTxPOW().hasBody()) {
								//Create an MMR set that will ONLY be used if the block is VALID..
								MMRSet mmrset = new MMRSet(zNode.getParent().getMMRSet());
								
								//Set this MMR..
								zNode.setMMRset(mmrset);
								
								//Check all the transactions in the block are correct..
								allok = getDB().checkFullTxPOW(zNode.getTxPow(), mmrset);
								
								//Check the root MMR..
								if(allok) {
									MiniData root = mmrset.getMMRRoot().getFinalHash();
									if(!row.getTxPOW().getMMRRoot().isEqual(root)) {
										allok = false;	
									}
								}
							}else {
								MinimaLogger.log("WARNING : sortBlockTreeNodeStates on no body TxPoW..! "+zNode.toString());
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
	 * Deep Copy a Node and it's children
	 * 
	 * @param zStartNode
	 * @return
	 */
	public static BlockTreeNode copyTreeNode(BlockTreeNode zStartNode) {
		//Create a STACK..
		NodeStack stack     = new NodeStack();
		NodeStack stackcopy = new NodeStack();
		
		//Now loop..
		BlockTreeNode curr     = zStartNode; 
		curr.mTraversedChild   = 0;
		
		//The Copy..
		BlockTreeNode rootcopy   = null;
		BlockTreeNode currparent = null; 
		BlockTreeNode currcopy   = null; 
		
        // traverse the tree 
        while (curr != null || !stack.isEmpty()) { 
        	while (curr !=  null) { 
            	//Copy the Node..
        		currcopy = new BlockTreeNode(curr);
        		if(rootcopy == null) {
        			rootcopy = currcopy;
        		}
        		if(currparent != null) {
        			currparent.addChild(currcopy);
        		}
        		
            	//Push on the stack..
            	stack.push(curr); 
            	stackcopy.push(currcopy);
            	
            	//Does it have children
            	int childnum = curr.mTraversedChild;
            	if(curr.getNumberChildren() > childnum) {
                	curr.mTraversedChild++;
                	
                	//Keep as the parent..
                	currparent = currcopy;
                			
                	curr = curr.getChild(childnum); 
                	curr.mTraversedChild = 0;
                }else {
                	curr = null;
                }
            } 
  
            //Current must be NULL at this point
            curr     = stack.peek();
            currcopy = stackcopy.peek();
            
            //Get the next child..
            int childnum = curr.mTraversedChild;
        	if(curr.getNumberChildren() > childnum) {
            	//Increment so next time a different child is chosen
        		curr.mTraversedChild++;
            	
        		//Keep as the parent..
            	currparent = currcopy;
            	
        		curr = curr.getChild(childnum); 
            	curr.mTraversedChild = 0;
            }else{
            	//We've seen all the children.. remove from the stack
            	stack.pop();
            	stackcopy.pop();
            	
            	//Reset the current to null
            	curr = null;
            }
        }
        
        return rootcopy;
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
	
	/**
	 * Recurse the whole tree and ru an action..
	 * 
	 * return object if something special found..
	 * 
	 * @param zNodeAction
	 * @return
	 */
	private BlockTreeNode _recurseTree(NodeAction zNodeAction) {
		//If nothing on chain return nothing
		if(getChainRoot() == null) {return null;}
				
		return _recurseTree(zNodeAction, getChainRoot());
	}
	
	private BlockTreeNode _recurseTree(NodeAction zNodeAction, BlockTreeNode zStartNode) {
		//Create a STACK..
		NodeStack stack = new NodeStack();
		
		//Now loop..
		BlockTreeNode curr   = zStartNode; 
		curr.mTraversedChild = 0;
		
        // traverse the tree 
        while (curr != null || !stack.isEmpty()) { 
        	while (curr !=  null) { 
            	//Run the Action
        		zNodeAction.runAction(curr);
        		
        		//Have we found what we were looking for..
        		if(zNodeAction.returnObject()) {
        			return zNodeAction.getObject();
        		}
            	
            	//Push on the stack..
            	stack.push(curr); 
            	
            	//Does it have children
            	int childnum = curr.mTraversedChild;
            	if(curr.getNumberChildren() > childnum) {
                	curr.mTraversedChild++;
                	curr = curr.getChild(childnum); 
                	curr.mTraversedChild = 0;
                }else {
                	curr = null;
                }
            } 
  
            //Current must be NULL at this point
            curr = stack.peek();
        	
            //Get the next child..
            int childnum = curr.mTraversedChild;
        	if(curr.getNumberChildren() > childnum) {
            	//Increment so next time a different child is chosen
        		curr.mTraversedChild++;
            	curr = curr.getChild(childnum); 
            	curr.mTraversedChild = 0;
            }else{
            	//We've seen all the children.. remove from the stack
            	stack.pop();
            	
            	//Reset the current to null
            	curr = null;
            }
        }
        
        return null;
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
	public BlockTreeNode getPastBlock(int zNumberFromTip) {
		MiniNumber cascnumber = mCascadeNode.getTxPow().getBlockNumber();
		BlockTreeNode current = mTip;
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
		//Use a previous block.. 
		BlockTreeNode starter = getPastBlock(GlobalParams.MINIMA_BLOCKS_SPEED_CALC.getAsInt());
		
		//Calculate to seconds..
		MiniNumber start      = starter.getTxPow().getTimeSecs();
		MiniNumber end        = mTip.getTxPow().getTimeSecs();
		MiniNumber timediff   = end.sub(start);
		
		//How many blocks..
		MiniNumber blockstart = starter.getTxPow().getBlockNumber();
		MiniNumber blockend   = mTip.getTxPow().getBlockNumber();
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
		//The Total..
		BigInteger totaldifficulty = new BigInteger("0");
		int numberofblocks=0;
		
		//Cycle back from the tip..
		BlockTreeNode starter   = getPastBlock(GlobalParams.MINIMA_BLOCKS_SPEED_CALC.getAsInt());
		MiniNumber minblock     = starter.getTxPow().getBlockNumber();
		BlockTreeNode current 	= mTip;
				
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
	}

	public static TxPoW createRandomTxPow() {
		TxPoW txpow = new TxPoW();
		txpow.setHeaderBodyHash();
		txpow.calculateTXPOWID();
		
		return txpow;
	}
	
	public static void main(String[] zArgs) {
		
		BlockTree tree = new BlockTree();
		
		TxPoW root = createRandomTxPow();
		BlockTreeNode rootnode = new BlockTreeNode(root);
		tree.setTreeRoot(rootnode);
		System.out.println("root : "+rootnode.getTxPowID().to0xString(10));
		
		//2 kids..
		TxPoW child = createRandomTxPow();
		BlockTreeNode treenode = new BlockTreeNode(child);
		rootnode.addChild(treenode);
		System.out.println("rootchild1 : "+treenode.getTxPowID().to0xString(10));
		
		TxPoW child4 = createRandomTxPow();
		BlockTreeNode treenode4 = new BlockTreeNode(child4);
		treenode.addChild(treenode4);
		System.out.println("child1child1 : "+treenode4.getTxPowID().to0xString(10));
		
		TxPoW child5 = createRandomTxPow();
		BlockTreeNode treenode5 = new BlockTreeNode(child5);
		treenode.addChild(treenode5);
		System.out.println("child1child2 : "+treenode5.getTxPowID().to0xString(10));
		
		TxPoW child6 = createRandomTxPow();
		BlockTreeNode treenode6 = new BlockTreeNode(child6);
		treenode.addChild(treenode6);
		System.out.println("child1child3 : "+treenode6.getTxPowID().to0xString(10));
		
		TxPoW child2 = createRandomTxPow();
		BlockTreeNode treenode2 = new BlockTreeNode(child2);
		rootnode.addChild(treenode2);
		System.out.println("rootchild2 : "+treenode2.getTxPowID().to0xString(10));
		
		TxPoW child3 = createRandomTxPow();
		BlockTreeNode treenode3 = new BlockTreeNode(child3);
		treenode2.addChild(treenode3);
		System.out.println("child2child1 : "+treenode3.getTxPowID().to0xString(10));
		
		tree.printTree();
		System.out.println();
		
		//Lets copy..
		BlockTreeNode copy = BlockTree.copyTreeNode(rootnode);
		BlockTree copytree = new BlockTree();
		copytree.setTreeRoot(copy);
	
		copytree.printTree();
		
		
		//Search for the child..
//		System.out.println("\nSearch for "+child3.getTxPowID().to0xString(10)+"\n\n");
//		BlockTreeNode find =  tree.findNode(child3.getTxPowID());
//		BlockTreeNode find =  tree.findNode(MiniData.getRandomData(5));
		
		
	}
	
	
}
