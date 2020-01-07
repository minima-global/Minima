package org.minima.utils.tests.cchain;

import java.util.ArrayList;

public class Cascader {

	//The Current Root..
	CChainBlock mRoot 		= null;
	CChainBlock mCurrent 	= null;
	
	//The Latest blocks at each level..
	CChainBlock[] mLatestBlocks;
	
	public Cascader() {
		mLatestBlocks = new CChainBlock[CChainBlock.MAX_LEVELS];
	}
	
	public Cascader(CChain zChain) {
		mLatestBlocks = new CChainBlock[CChainBlock.MAX_LEVELS];
	
		//Get a full list and add.
		ArrayList<CChainBlock> blocks = zChain.getAsList();
		for(CChainBlock block : blocks) {
			addBlock(block);
		}
	}
	
	public void addBlock(CChainBlock zBlock) {
		//What Level is the Block
		int level = zBlock.getFinalLevel();
		
		//Check if root
		if(mCurrent == null) {
			mRoot = zBlock;
			mRoot.mParent = null;
			
		}else {
			//Old Level
			int oldlevel = mCurrent.getFinalLevel();
			
			if(level <= oldlevel) {
				//Do Nothing just add
				mCurrent.mChild = zBlock;
				zBlock.mParent 	= mCurrent;
			}else {
				//It's greater than
				CChainBlock last = mLatestBlocks[level];
				
				if(last == null) {
					//Back to root..
					mRoot = zBlock;
					mRoot.mParent = null;
				}else {
					last.mChild = zBlock;
					zBlock.mParent = last;
				}
			}
		}
		
		//Set the Latest Blocks..
		for(int i=level;i>=0;i--) {
			mLatestBlocks[i] = zBlock;
		}
		
		//New Current Block
		mCurrent = zBlock;
	}
	
	public CChain getAsCChain() {
		CChain chain = new CChain();
		
		ArrayList<CChainBlock> blocks = getAsList();
		
		for(CChainBlock block : blocks) {
			chain.addBlock(block);
		}
		
		return chain;
	}
	
	public ArrayList<CChainBlock> getAsList(){
		ArrayList<CChainBlock> ret = new ArrayList<>();
	
		if(mRoot != null) {
			//Get the Current first node
			CChainBlock curr = mRoot;
			ret.add(curr);
			
			//Now start adding to the list
			while(curr.mChild != null) {
				curr = curr.mChild;
				ret.add(curr);
			}
		}
		 
		return ret;
	}
	
	public CChainBlock getRoot() {
		return mRoot;
	}
}
