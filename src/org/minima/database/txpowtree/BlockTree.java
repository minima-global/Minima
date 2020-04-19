package org.minima.database.txpowtree;

import java.math.BigInteger;
import java.util.ArrayList;

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
	BlockTreeNode mTip;
	
	/**
	 * The Block beyond which you are cascading and parents may be of a higher level
	 */
	BlockTreeNode mCascadeNode;
	
	/**
	 * Main Constructor
	 */
	public BlockTree() {}
	
	public void setTreeRoot(BlockTreeNode zNode) {
		zNode.setParent(null);
		mRoot 			= zNode;
		mTip 			= mRoot;
		mCascadeNode 	= mRoot;
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
		
		//Otherwise get the parent block and add this to that
		MiniData prevblock = zNode.getTxPow().getParentID();
		
		//Find the parent block.. from last uncascaded node onwards
		BlockTreeNode parent = findNode(prevblock);
		
		//Do we have a parent..
		if(parent == null) {
			//No direct parent..  add to the pool and ask for parent
			return false;
		}
		
		//Check after the cascade node..
		if(zNode.getTxPow().getBlockNumber().isLessEqual(getCascadeNode().getTxPow().getBlockNumber())) {
			MinimaLogger.log("BlockTree : BLOCK PAST CASCADE NODE.. "+zNode.getTxPow());
			return false;
		}
		
		//It's OK - add it
		parent.addChild(zNode);

		//It's been added
		return true;
	}
	
	/**
	 * Adds the node regardless of the parents.. used when cascading
	 * @param zNode
	 * @return
	 */
	public void hardAddNode(BlockTreeNode zNode, boolean zLinkAll) {
		if(mRoot == null) {
			setTreeRoot(zNode);
			zNode.setParent(null);
			return;
		}
		
		//Add to the end..
		mTip.addChild(zNode);
		
		//Link the MMR..
		if(zNode.getMMRSet() != null) {
			if(zLinkAll) {
				if(mTip.getTxPowID().isEqual(zNode.getTxPow().getParentID())) {
					//Correct Parent.. can link the MMR!
					zNode.getMMRSet().setParent(mTip.getMMRSet());
				}
			}else {
//				if(!mTip.isCascade()) {
//					zNode.getMMRSet().setParent(mTip.getMMRSet());
//				}
				
				if(!mTip.isCascade() && mTip.getTxPowID().isEqual(zNode.getTxPow().getParentID())) {
					//Correct Parent.. can link the MMR!
					zNode.getMMRSet().setParent(mTip.getMMRSet());
				}
			}
		}
				
		//Move on..
		mTip = zNode;
	}
	
	public void hardSetCascadeNode(BlockTreeNode zNode) {
		mCascadeNode = zNode;
	}
	
	/**
	 * Resets the weights in the tree
	 */
	public void resetWeights() {
		//First default them
		_zeroWeights(getChainRoot());
		
		//Start at root..
		_cascadeWeights(getChainRoot());
		
		//And get the tip..
		mTip = getHeaviestBranchTip(getChainRoot());		
	}
	
	private void _zeroWeights(BlockTreeNode zNode) {
		zNode.resetCurrentWeight();
		ArrayList<BlockTreeNode> children = zNode.getChildren();
		for(BlockTreeNode child : children) {
			_zeroWeights(child);
		}
	}
	
	private void _cascadeWeights(BlockTreeNode zNode) {
		//Only add valid blocks
		if(zNode.getState() != BlockTreeNode.BLOCKSTATE_VALID) {
			return;
		}
		
		//The weight of this block
		BigInteger weight = zNode.getWeight();
		
		//Add to all the parents..
		BlockTreeNode parent = zNode.getParent();
		while(parent != null) {
			parent.addToTotalWeight(weight);
			parent = parent.getParent();
		}
		
		//Now scour the children
		ArrayList<BlockTreeNode> children = zNode.getChildren();
		for(BlockTreeNode child : children) {
			_cascadeWeights(child);
		}
	}
	
	
	/**
	 * Pick the GHOST heaviest Branch of the tree
	 * @param zStartNode
	 * @return
	 */
	private BlockTreeNode getHeaviestBranchTip(BlockTreeNode zStartNode) {
		if(zStartNode.getNumberChildren()>0) {
			//Max weight Node..
			BlockTreeNode max = null;
			
			//Get the heaviest child branch
			ArrayList<BlockTreeNode> children = zStartNode.getChildren();
			for(BlockTreeNode node : children) {
				//ONLY VALID BLOCKS
				if(node.getState() == BlockTreeNode.BLOCKSTATE_VALID) {
					if(max == null) {
						max = node;
					}else {
						if(node.getTotalWeight().compareTo(max.getTotalWeight()) > 0) {
							max = node;
						}
					}
				}
			}

			if(max != null) {
				return getHeaviestBranchTip(max);
			}
		}
		
		return zStartNode;
	}
	
	/**
	 * Find a specific node in the tree
	 * 
	 * @param zTxPOWID
	 * @return
	 */
	public BlockTreeNode findNode(MiniData zTxPOWID) {
		if(getChainRoot() == null) {
			return null;
		}
		
		return _findNode(getChainRoot(), zTxPOWID);
	}
	
	private BlockTreeNode _findNode(BlockTreeNode zRoot, MiniData zTxPOWID) {
		//Check..
		if(zRoot.getTxPowID().isEqual(zTxPOWID)) {
			return zRoot;
		}
		
		//Search the Children..
		ArrayList<BlockTreeNode> children = zRoot.getChildren();
		
		for(BlockTreeNode child : children) {
			BlockTreeNode found = _findNode(child, zTxPOWID);
		
			if(found != null) {
				return found;
			}
		}
		
		return null;
	}
	
	public BlockTreeNode getOnChainBlock(MiniNumber zBlockNumber) {
			return _getOnChainBlock(zBlockNumber, mTip);
	}
	
	private BlockTreeNode _getOnChainBlock(MiniNumber zBlockNumber, BlockTreeNode zTip) {
		if(zTip == null) {
			return null;
		}
		
		if(zTip.getTxPow().getBlockNumber().isEqual(zBlockNumber)) {
			return zTip;
		}
		
		return _getOnChainBlock(zBlockNumber, zTip.getParent());
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
	 * Get the Chain Speed..
	 * 
	 * Calculated as the different between the cascade node and the tip..
	 */
	public MiniNumber getChainSpeed() {
		//Time difference between the cascade node and the tip
		MiniNumber start      = mCascadeNode.getTxPow().getTimeSecs();
		MiniNumber end        = mTip.getTxPow().getTimeSecs();
		MiniNumber timediff   = end.sub(start);
		
		//How many blocks..
		MiniNumber blockstart = mCascadeNode.getTxPow().getBlockNumber();
		MiniNumber blockend   = mTip.getTxPow().getBlockNumber();
		MiniNumber blockdiff  = blockend.sub(blockstart); 
		
		//So.. 
		if(timediff.isEqual(MiniNumber.ZERO)) {
			return MiniNumber.ONE;
		}
		MiniNumber speed    = blockdiff.div(timediff);
		
		return speed;
	}
	
	/**
	 * Get the current average difficulty
	 */
	public BigInteger getAvgChainDifficulty() {
		//The Total..
		BigInteger total = new BigInteger("0");
		
		//Cycle back from the tip..
		MiniData casc 			= mCascadeNode.getTxPowID();
		BlockTreeNode current 	= mTip;
		int num=0;
		while(current != null) {
			//Add to the total
			total = total.add(current.getTxPow().getBlockDifficulty().getDataValue());
			num++;
			
			if(current.getTxPowID().isEqual(casc)) {
				//It's the final node.. quit
				break;
			}
			
			//Get thew parent
			current = current.getParent();
		}
		
		if(num == 0) {
			return BigInteger.ZERO;
		}
		
		//The Average
		BigInteger avg = total.divide(new BigInteger(""+num));
		
		return avg;
	}

	public void clearTree() {
		mRoot 			= null;
		mTip 			= null;
		mCascadeNode 	= null;
	}

}
