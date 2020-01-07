package org.minima.utils.tests.cchain;

public class CChainBlock {

	//At the lowest level.. 
	public int 		mBaseBlockNumber;
	public double 	mBasePrevBlockHash;
	
	//A random hash
	public double 	mBlockHash;
	
	public int mMaxBlockLevel;
	public int mCurrentBlockLevel;
	
	public static final int MAX_LEVELS = 2;
	public CChainLevel[] mBlockLevels;
	
	//On CChain what this becomes
	public double mFinalWeight = 0;
	public double mFinalLevel  = 0;
	
	//The parent and child of this block
	public CChainBlock mParent = null;
	public CChainBlock mChild  = null;
	
	public CChainBlock() {
		mBlockLevels	 	= new CChainLevel[MAX_LEVELS];
		mCurrentBlockLevel 	= 0;
	}
	
	@Override
	public String toString() {
		int level 	= getFinalLevel();
		String s 	= "[LEV:"+level+" / "+mCurrentBlockLevel+"] BaseNum:"+mBaseBlockNumber+" / "+CChainSim.NUMBER_BLOCKS+" Hash:"+mBlockHash+" Prev:"+mBasePrevBlockHash;
		
		//The Levels..
		for(int i=0;i<=mCurrentBlockLevel;i++) {
			s+="\n"+mBlockLevels[i];
		}
		
		return s;
	}
	
	/**
	 * Return the Final Level this block is
	 * @return
	 */
	public int getFinalLevel() {
		//The current level
		int level=0;
		
		for(int i=0;i<=mCurrentBlockLevel;i++) {
			//Get the Block data
			CChainLevel lev = mBlockLevels[i];
			
			//Is it possible.. 0 is impossible
			if(lev.mBlockDifficulty != 0) {
				
				//Now create a double value and check
				double diff = 1.0 / lev.mBlockDifficulty;
				if(mBlockHash <= diff) {
					level = lev.mBlockLevel;
				}
			}
		}
		
		return level;
	}
}
