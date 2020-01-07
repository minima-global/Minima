package org.minima.utils.tests.cchain;

public class CChainLevel {

	public int 		mBlockLevel;
	
	public int 		mBlockNumber;
	
	public double 	mPreviousBlock;
	
	/**
	 * A block Difficulty of 0 means impossible.
	 * You can only find a level block if a block 1 level lower exists
	 */
	public double 	mBlockDifficulty = 0;

	public CChainLevel() {}
	
	@Override
	public String toString() {
		if(mBlockDifficulty!=0) {
			return "N:"+mBlockNumber+" L:"+mBlockLevel+" D:"+mBlockDifficulty+" ( < "+(1.0 / mBlockDifficulty)+") P:"+mPreviousBlock; 
		}else {
			return "N:"+mBlockNumber+" L:"+mBlockLevel+" D:"+mBlockDifficulty+" P:"+mPreviousBlock; 
		}
		 
	}
}
