package org.minima.utils.tests.cascader;

import org.minima.utils.MinimaLogger;

public class Block {

	//At the lowest level.. 
	public int 		mBaseBlockNumber;
	public double 	mBasePrevBlockHash;
	
	//A random hash
	public double 	mBlockHash;
	
	public int mMaxBlockLevel;
	public int mCurrentBlockLevel;
	
	public BlockLevel[] mBlockLevels;
	
	//On CChain what this becomes
	public double mFinalWeight = 0;
	public double mFinalLevel  = 0;
	
	public Block() {
		//Far too large.. 
		mBlockLevels = new BlockLevel[32];
		mBlockHash 	 = 0;
	}

	public void printLevels() {
		MinimaLogger.log("Hash : "+mBlockHash+" Level:"+getBlockLevel()+" Current:"+mCurrentBlockLevel+" MaxLevel:"+mMaxBlockLevel);
		
		for(int lev=0;lev<=mCurrentBlockLevel;lev++) {
			String desc = "[ lev:"+lev+" blocknumber:"+mBlockLevels[lev].mBlockNumber
					+", prevHash:"+mBlockLevels[lev].mPreviousBlock
					+", Diff:"+mBlockLevels[lev].mBlockDifficulty+" ]";
			MinimaLogger.log(desc);
		}
		MinimaLogger.log("");
	}
		
	@Override
	public String toString() {
	
		return "[ ("+mBlockLevels[0].mBlockNumber+") "
		+getBlockLevel()+" / "+mCurrentBlockLevel+" / "+mMaxBlockLevel+", "
		+mBlockHash+" , FINAL WEIGHT:"+mFinalWeight+" ]";

		
//		int blev = getBlockLevel();
//		return "[ ("+mBlockLevels[0].mBlockNumber+") "
//					+blev+" / "+mMaxBlockLevel+", "
//					+mBlockHash+" ,Diff:"+mBlockLevels[blev].mBlockDifficulty+",  FINAL WEIGHT:"+mFinalWeight+" ]";
	}
	
	public boolean checkBlockLevel(int zLevel) {
		if(mCurrentBlockLevel >= zLevel) {
			double diff = 1.0 / mBlockLevels[zLevel].mBlockDifficulty;
			return (mBlockHash <= diff);
		}
		
		return false;
	}
	
	public int getBlockLevel() {
		//The current level
		int level=0;
		
		for(int i=0;i<=mCurrentBlockLevel;i++) {
			//Get the Block data
			BlockLevel lev = mBlockLevels[i];
			
			//Is it enough..
			if(lev.mBlockDifficulty != 0) {
				double diff = 1.0 / lev.mBlockDifficulty;
				if(mBlockHash <= diff) {
					level = lev.mBlockLevel;
				}
			}
		}
		
		return level;
	}
}
