package org.minima.database.txpowtree;

import java.util.ArrayList;

import org.minima.GlobalParams;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.greet.SyncPacket;
import org.minima.system.Main;
import org.minima.system.brains.BackupManager;

public class CascadeTree {

	BlockTree mMainTree;
	
	BlockTree mCascadeTree;
	
	ArrayList<BlockTreeNode> mRemovals;
	
	public CascadeTree(BlockTree zMainTree) {
		mMainTree = zMainTree;
		mRemovals   = new ArrayList<>();
	}
	
	public BlockTree getCascadeTree() {
		return mCascadeTree;
	}
	
	public ArrayList<BlockTreeNode> getRemoved(){
		return mRemovals;
	}
	
	public void cascadedTree() {
		//Reset the removals
		mRemovals = new ArrayList<>();
		
		//At worst
		mCascadeTree = mMainTree;
		
		//Get the Old Tip
		BlockTreeNode oldtip = mMainTree.getChainTip();
		if(oldtip == null) {
			return;
		}
		
		//Are we long enough..
		BlockTreeNode cascadenode = mMainTree.getCascadeNode();
		MiniNumber cascnumber = cascadenode.getBlockNumber();
		MiniNumber totlength = oldtip.getBlockNumber().sub(cascadenode.getBlockNumber());
		if(totlength.isLess(GlobalParams.MINIMA_CASCADE_START_DEPTH)) {
			return;
		}
		
		//Get the new cascade node..
		BlockTreeNode newfulltree = oldtip;
		int counter = 1;
		int max = GlobalParams.MINIMA_CASCADE_START_DEPTH.getAsInt();
		while(counter < max) {
			newfulltree = newfulltree.getParent();
			counter++;
		}
		
		//The final cascaded tree - going to be new
		mCascadeTree = new BlockTree();
				
		//All this we keep
		BlockTreeNode fullkeep = newfulltree;
		
		//The rest of the tree.. that we CAN cascade
		BlockTreeNode newcascade  = newfulltree.getParent();
		
		//Now copy all the MMR data to the old cascade..
		MiniNumber minblock = newcascade.getMMRSet().copyAllParentKeepers(cascadenode.getBlockNumber());
		
		//Get the Backup Manager..
		BackupManager backup = Main.getMainHandler().getBackupManager();
		
		//Get all the blocks we have to save as .block..
		BlockTreeNode savenode = newcascade.getParent();
		while(savenode!=null && savenode.getBlockNumber().isMoreEqual(minblock)) {
			//Tell the backup manager to save this file..
			backup.backupSaveTempBlock(savenode.getBlockNumber(), savenode.getTxPow().getTxPowID().to0xString());
			
			//Get the Parent
			savenode = savenode.getParent();
		}
		
		//Now add all that
		ArrayList<BlockTreeNode> cascnodes = new ArrayList<>();
		while(newcascade != null) {
			//Get the parent..
			BlockTreeNode pnode = newcascade.getParent();
			
			//reset the node..
			newcascade.clearParentChildren();
			newcascade.setCascade(true);
			newcascade.setState(BlockTreeNode.BLOCKSTATE_VALID);
			
			//Add to our list
			cascnodes.add(newcascade);
			
			//Go up the tree..
			newcascade = pnode;
		}
		
		//Now have 2 parts of the tree.. a full part and a cascade part
		ArrayList<BlockTreeNode> finalnodes = new ArrayList<>();
		
		//Now cycle through the nodes removing a higher level if enough of the lower levels are there..
		int casclevel = 0;
		int totlevel  = 0;
		for(BlockTreeNode node : cascnodes) {
			int superlev = node.getSuperBlockLevel();
			
			//Are we above the minimum power
			if(superlev>=casclevel) {
				
				//METHOD1 - super simple
				node.setCurrentLevel(casclevel);
				
				//Add to the final list.. This node made it..
				finalnodes.add(0,node);
				
				//Increase node count at this level
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
				
			}else{
				//Add to the removals..
				mRemovals.add(node);
			}
		}	
			
		//Now add all this to the final tree
		for(BlockTreeNode node : finalnodes) {
			//Add..
			mCascadeTree.hardAddNode(node, false);
			mCascadeTree.hardSetCascadeNode(node);
		}
				
		//Add the rest
		mCascadeTree.hardAddNode(fullkeep, true);
		
		//And sort the weights
		mCascadeTree.resetWeights();
		
		//And clear it out..
		mCascadeTree.clearCascadeBody();
	}
}
