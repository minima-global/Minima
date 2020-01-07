package org.minima.utils.tests.cchain;

import java.util.ArrayList;

public class CChain {

	/**
	 * The root node of the whole chain
	 */
	CChainBlock mRoot;
	
	/**
	 * Current latest Node
	 */
	CChainBlock mCurrent;
	
	public CChain() {}
	
	public CChainBlock getRoot() {
		return mRoot;
	}
	
	public CChainBlock getCurrent() {
		return mCurrent;
	}
	
	public void addBlock(CChainBlock zBlock) {
		if(mCurrent == null) {
			//It's root
			mRoot = zBlock;
			
			mRoot.mParent = null;
			mRoot.mChild  = null;
		}else {
			zBlock.mParent  = mCurrent;
			mCurrent.mChild = zBlock;
		}
		
		//Set the new Current
		mCurrent = zBlock;
	}
	
	public void setCurrent(CChainBlock zBLock) {
		mCurrent = zBLock;
	}
	
	/**
	 * Get the entire current chain as an ArrayList
	 * @return
	 */
	public ArrayList<CChainBlock> getAsList(){
		ArrayList<CChainBlock> ret = new ArrayList<>();
	
		//Get the Current first node
		CChainBlock curr = mRoot;
		ret.add(curr);
		
		//Now start adding to the list
		while(curr.mChild != null) {
			curr = curr.mChild;
			ret.add(curr);
		}
		 
		return ret;
	}
	
	/**
	 * Returns a boolean array of all the;levels that are present
	 */
	public boolean[] getLevels() {
		boolean[] ret = new boolean[CChainBlock.MAX_LEVELS];
		for(int i=0;i<CChainBlock.MAX_LEVELS;i++) {
			ret[i] = false;
		}
		
		ArrayList<CChainBlock> blocks = getAsList();
		for(CChainBlock block : blocks) {
			ret[block.getFinalLevel()] = true;
		}
		
		return ret;
	}
	
	public int getSize() {
		return getAsList().size();
	}
	
	public int getMaxLevel() {
		int max = 0;
		ArrayList<CChainBlock> blocks = getAsList();
		for(CChainBlock block : blocks) {
			int lev = block.getFinalLevel();
			if(lev>max) {
				max = lev;
			}
		}
		return max;
	}

	public static double getTotalLevelDiff(CChainBlock zBlock, int zLevel) {
		//What Level is this Block
		double diff = 0;
		
		//Now cycle back until you find another block of this level..
		CChainBlock parent = zBlock.mParent;
		while(parent != null) {
			int plevel = parent.getFinalLevel();
			if(plevel<zLevel) {
				diff += parent.mBlockLevels[plevel].mBlockDifficulty; 
				
			}else {
				//That's it.. lets jump..
				break;
			}
			
			parent = parent.mParent;
		}
		
		return diff+1;
	}
	
	public String toLevelChain() {
		String ret = new String();
		
		ArrayList<CChainBlock> blocks = getAsList();
		
		int current = blocks.get(0).getFinalLevel();
		int lev 	= current;
		int tot 	= 0;
		
		boolean added = false;
		for(CChainBlock block : blocks) {
			lev = block.getFinalLevel();
//			ret += ","+lev+"["+block.mBlockLevels[lev].mBlockDifficulty+"]";
			
			if(lev == current) {
				tot++;
				added = false;
			}else {
				ret += ",L:"+current+" * "+tot;
				tot=1;
				current = lev;
				added = false;
			}	
		}
		
		if(!added) {
			ret += ",L:"+lev+" * "+tot;
		}
		
		return ret;
	}
	
	public double getTotalPOW() {
		double ret = 0;
		
		ArrayList<CChainBlock> blocks = getAsList();
		for(CChainBlock block : blocks) {
			int lev = block.getFinalLevel();
			ret += block.mBlockLevels[lev].mBlockDifficulty;
		}
		
		return ret;
	}
	
	/**
	 * Cascade Chain
	 * 
	 * The Main scrunching function that removes blocks and compresses the chain
	 * Running this function again on a compressed chain should have no effect.
	 */
	public CChain cascadeChain() {
		//The new smaller chain
		CChain newChain = new CChain();
		
//		//A list of the latest block at each level..
//		CChainBlock[] latestLevelBlocks = new CChainBlock[CChainBlock.MAX_LEVELS];
//		for(int i=0;i<CChainBlock.MAX_LEVELS;i++) {
//			//To start all set to null
//			latestLevelBlocks[i] = null;
//		}
//		
//		//Get the current root block
//		CChainBlock currentBlock 		= getRoot();
//		int currentLevel		 		= currentBlock.getFinalLevel();
//
//		//This block and all blocks below jump to here
//		for(int i=currentLevel;i>=0;i--) {
//			latestLevelBlocks[currentLevel] = currentBlock;
//		}
//		
//		//To start the root is the same
//		newChain.addBlock(currentBlock);
//		
//		//Now get the next Child..
//		CChainBlock child = currentBlock.mChild;
//		while(child != null) {
//			//What level..
//			int clevel = child.getFinalLevel();
//			
//			//scrunched..
//			CChainBlock lastblock = latestLevelBlocks[clevel];
//			
//			//Check to see if we _scrunch_
//			if(clevel == currentLevel) {
//				//No change.. just add
//				newChain.addBlock(child);
//				
//			}else if(clevel > currentLevel) {
//				//Is there one..
//				if(lastblock == null) {
//					//Jump to root..
//					newChain.setRoot(child);
//					
//					child.mParent = null;
//					child.mChild  = null;
//				
//					//All Blocks Removed
//					for(int i=0;i<CChainBlock.MAX_LEVELS;i++) {
//						//To start all set to null
//						latestLevelBlocks[i] = null;
//					}
//					
//				}else {
//					//Jump back to that..
//					lastblock.mChild = child;
//					child.mParent = lastblock;
//					
//					newChain.setCurrent(child);
//				}
//				
//			}else if(clevel < currentLevel){
//				//Set to the chain..
//				newChain.addBlock(child);
//			}
//			
//			//Set the new Latest Block
//			//This block and all blocks below jump to here
//			for(int i=currentLevel;i>=0;i--) {
//				latestLevelBlocks[clevel] = currentBlock;
//			}
//			
//			//Are there more..
//			currentLevel    = clevel;
//			currentBlock 	= child;
//			child 			= currentBlock.mChild;
//		}
		
		return newChain;
	}
	
}
