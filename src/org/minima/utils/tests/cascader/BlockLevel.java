package org.minima.utils.tests.cascader;

public class BlockLevel {

	public int 		mBlockLevel;
	
	public int 		mBlockNumber;
	
	public double 	mPreviousBlock;
	
	public double 	mBlockDifficulty;

	public BlockLevel() {
		
	}
	
	@Override
	public String toString() {
		return "N:"+mBlockNumber+" L:"+mBlockLevel+" D:"+mBlockDifficulty+" P:"+mPreviousBlock;  
	}
}
