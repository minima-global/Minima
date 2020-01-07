package org.minima.database.txpowtree;

import java.util.ArrayList;

import org.minima.GlobalParams;
import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMRSet;
import org.minima.database.txpowdb.TxPOWDBRow;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.MinimaLogger;

public class CascadeTree {

	MinimaDB mDB;
	
	BlockTree mMainTree;
	
	BlockTree mCascadeTree;
	
	ArrayList<BlockTreeNode> mRemovals;
	
	public CascadeTree(BlockTree zMainTree, MinimaDB zDB) {
		mMainTree = zMainTree;
		mRemovals = new ArrayList<>();
		mDB = zDB;
	}
	
	public BlockTree getCascadeTree() {
		return mCascadeTree;
	}
	
	public ArrayList<BlockTreeNode> getRemoved(){
		return mRemovals;
	}
	
	public void recurseParentMMR(MiniNumber zCascade, MMRSet zNode) {
		if(zNode.getBlockTime().isMore(zCascade.increment())) {
			//Do all the parents
			recurseParentMMR(zCascade, zNode.getParent());
		}
			
		//The you do it..
		zNode.copyParentKeepers();
	}
	
	public ArrayList<BlockTreeNode> cascadedTree() {
		//Reset the removals
		mRemovals = new ArrayList<>();
		
		//The final cascaded tree
		mCascadeTree = new BlockTree();
				
		//Get the current tip
		BlockTreeNode oldtip      = mMainTree.getChainTip();
		BlockTreeNode oldcascade  = mMainTree.getCascadeNode();
		MiniNumber casc           = oldcascade.getTxPow().getBlockNumber();
		
		//First get the block PRE_CASCADE_CHAIN_LENGTH back..
		int counter=0;
		while((oldtip!=null) && (counter<GlobalParams.MINIMA_CASCADE_DEPTH) && (oldtip.getTxPow().getBlockNumber().isMore(casc)) ) {
			counter++;
			oldtip = oldtip.getParent();
		}
		
		//Tree is not long enough to cascade
		if(oldtip == null) {
			mCascadeTree = mMainTree;
			return mRemovals;
		}
		
		//All this we keep
		BlockTreeNode fullkeep = copyNodeTree(oldtip);
		
		//The rest of the tree.. that we CAN cascade
		BlockTreeNode newcascade  = oldtip.getParent();
		if(newcascade != null && newcascade.getMMRSet()!=null){
			//Sort the MMR.. DO this on a cascade node so not added to the user syncup.
			recurseParentMMR(casc,newcascade.getMMRSet());
		}
				
		//Now add all that
		ArrayList<BlockTreeNode> cascnodes = new ArrayList<>();
		while(newcascade != null) {
			//Create a new cascade node..
			BlockTreeNode node = new BlockTreeNode(newcascade);
			node.setCascade(true);
			node.setState(BlockTreeNode.BLOCKSTATE_VALID);
			
			//Add a node
			cascnodes.add(0,node);
			
			//Go up the tree..
			newcascade = newcascade.getParent();
		}
		
		//Now have 2 parts of the tree.. a full part and a cascade part
		ArrayList<BlockTreeNode> finalnodes = new ArrayList<>();
		
		//Remove all of these..
		for(BlockTreeNode node : cascnodes) {
			//Add the node..
			finalnodes.add(node);
			
			//Now cascade
			finalnodes = removeLowerLevels(finalnodes, node.getSuperBlockLevel());
		}
			
		//Now add all this to the final tree
		for(BlockTreeNode node : finalnodes) {
			//Create a new Node..
			BlockTreeNode copy = new BlockTreeNode(node);
			
			//Add..
			mCascadeTree.hardAddNode(copy, false);
			
			//It's a cascader
			mCascadeTree.hardSetCascadeNode(copy);
		}
				
		//Add the rest
		mCascadeTree.hardAddNode(fullkeep, false);
		
		//And sort the weights
		mCascadeTree.resetWeights();
		
		return mRemovals;
	}
	
	private ArrayList<BlockTreeNode> removeLowerLevels(ArrayList<BlockTreeNode> zCurrent, int zMinLevel){
		ArrayList<BlockTreeNode> ret = new ArrayList<>();
		
		//Do we keep or do we discard..
		for(BlockTreeNode node : zCurrent) {
			if(node.getSuperBlockLevel() >= zMinLevel) {
				//Up the Current Level
				node.setCurrentLevel(node.getSuperBlockLevel());
				
				//Create new node.. 
				BlockTreeNode copy = new BlockTreeNode(node);
				ret.add(copy);
			}else {
				TxPOWDBRow row = mDB.getTxPOWRow(node.getTxPowID());
				
				//Discard.. no longer an onchain block..
				row.setOnChainBlock(false);
				
				//Add to the removals..
				mRemovals.add(node);
			}
		}
		
		return ret;
	}
	
	/**
	 * Deep copy a Block treenode..
	 * 
	 * @param zOriginal
	 * @return
	 */
	private BlockTreeNode copyNodeTree(BlockTreeNode zOriginal) {
		BlockTreeNode copy = new BlockTreeNode(zOriginal);
		
		ArrayList<BlockTreeNode> children = zOriginal.getChildren();
		for(BlockTreeNode child : children) {
			BlockTreeNode childcopy = copyNodeTree(child);
			copy.addChild(childcopy);
		}
		
		return copy;
	}
	
	
}
