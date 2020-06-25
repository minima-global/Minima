package org.minima.database.txpowtree;

import java.util.ArrayList;

import org.minima.GlobalParams;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

public class CascadeTree {

	BlockTree mMainTree;
	
	BlockTree mCascadeTree;
	
	ArrayList<BlockTreeNode> mRemovals;
	
	public CascadeTree(BlockTree zMainTree) {
		mMainTree = zMainTree;
		mRemovals = new ArrayList<>();
	}
	
	public BlockTree getCascadeTree() {
		return mCascadeTree;
	}
	
	public ArrayList<BlockTreeNode> getRemoved(){
		return mRemovals;
	}
	
	public ArrayList<BlockTreeNode> cascadedTree() {
		//Reset the removals
		mRemovals = new ArrayList<>();
		
		//The final cascaded tree
		mCascadeTree = new BlockTree();
		
		//Is it empty..
		BlockTreeNode oldtip      = mMainTree.getChainTip();
		if(oldtip == null) {
			return mRemovals;
		}
		
		//Store this..
		MiniData oldtiptxpowid = oldtip.getTxPowID();
		
		//Get the current block 1 above the cascade
		MiniNumber cascinc = mMainTree.getCascadeNode().getTxPow().getBlockNumber().increment();
		
		//First get the block PRE_CASCADE_CHAIN_LENGTH back..
		int counter=0;
		while(  (oldtip!=null) && 
				(counter<GlobalParams.MINIMA_CASCADE_START_DEPTH) && 
				(oldtip.getTxPow().getBlockNumber().isMore(cascinc)) ) {
			counter++;
			oldtip = oldtip.getParent();
		}
		
		//Tree is not long enough to cascade
		if(oldtip == null) {
			mCascadeTree = mMainTree;
			return mRemovals;
		}
		
		//All this we keep
		BlockTreeNode fullkeep = BlockTree.copyTreeNode(oldtip);
		
		//The rest of the tree.. that we CAN cascade
		BlockTreeNode newcascade  = oldtip.getParent();
				
		//Now add all that
		ArrayList<BlockTreeNode> cascnodes = new ArrayList<>();
		while(newcascade != null) {
			//Create a new cascade node..
			BlockTreeNode node = new BlockTreeNode(newcascade);
			node.setCascade(true);
			node.setState(BlockTreeNode.BLOCKSTATE_VALID);
			
			//Add a node
			cascnodes.add(node);
			
			//Go up the tree..
			newcascade = newcascade.getParent();
		}
		
		//Now have 2 parts of the tree.. a full part and a cascade part
		ArrayList<BlockTreeNode> finalnodes = new ArrayList<>();
		
		//Now cycle through the nodes removing a higher level if enough of the lower levels are there..
		int casclevel=0;
		int totlevel = 0;
		boolean moveup = false;
		for(BlockTreeNode node : cascnodes) {
			int superlev = node.getSuperBlockLevel();
			
			//Are we above the minimum power
			if(superlev>=casclevel) {
				
				//METHOD1 - super simple
				node.setCurrentLevel(casclevel);
				finalnodes.add(0,node);
				totlevel++;
				
				//Keep at least this many at each level..
				if(totlevel>=GlobalParams.MINIMA_CASCADE_LEVEL_NODES) {
					if(casclevel<GlobalParams.MINIMA_CASCADE_LEVELS-1) {
						casclevel++;
						totlevel = 0;
					}else {
						//We've reached the top level.. keep them ALL!
						//..
					}
				}
				
				//METHOD2 - slightly better
//				node.setCurrentLevel(casclevel);
//				finalnodes.add(0,node);
//				totlevel++;
//				
//				//Keep at least this many at each level..
//				if(totlevel>=GlobalParams.MINIMA_MINUMUM_CASCADE_LEVEL) {
//					moveup = true;
//				}
//				
//				//Are we going up a level..
//				if(moveup && (superlev >= casclevel+1)) {
//					//Allow Move up a level.. when you hit the next valid node..
//					casclevel++;
//					totlevel = 0;
//					moveup   = false;	
//				}
				
//				//METHOD3 - wait for a higher level block before removing the old..
//				node.setCurrentLevel(casclevel);
//				finalnodes.add(0,node);
//				totlevel++;
//				
//				//Keep at least this many at each level..
//				if(totlevel>GlobalParams.MINIMA_MINUMUM_CASCADE_LEVEL_NODES) {
//					moveup = true;
//				}
//				
//				//Are we going up a level..
//				if(moveup && (superlev > casclevel)) {
//					//Allow Move up a level.. when you hit the next valid node..
//					casclevel++;
//					node.setCurrentLevel(casclevel);
//					totlevel = 1;
//					moveup   = false;	
//				}
				
			}else{
				//Add to the removals..
				mRemovals.add(node);
			}
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
		mCascadeTree.hardAddNode(fullkeep, true);
		
		//Find the old tip.. makes the reset weight 1000x faster..
		mCascadeTree.mTip = mCascadeTree.findNode(oldtiptxpowid,true);
		
		//And sort the weights
		mCascadeTree.resetWeights();
		
		return mRemovals;
	}
}
