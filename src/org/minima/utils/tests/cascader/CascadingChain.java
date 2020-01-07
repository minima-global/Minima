package org.minima.utils.tests.cascader;

import java.util.ArrayList;
import java.util.Random;

import org.minima.utils.MinimaLogger;

public class CascadingChain {

	public static int NUMBER_BLOCKS 		= 10;
	public static int NUMBER_SIMULATIONS 	= 1;
	public static int MAX_BLOCK_LEVEL		= 2;
	
	public static void main(String[] zArgs) {
		//The simulated HASH function
		Random rand = new Random();
		
		//The chains - we keep both to compare at the end
		ArrayList<Block> Blocks 			= new ArrayList<>();
	
		//All the current block numbers at each level
		int[] blockNumbers 		= new int[MAX_BLOCK_LEVEL+1];
		Block[] previousBlocks 	= new Block[MAX_BLOCK_LEVEL+1];
		for(int i=0;i<=MAX_BLOCK_LEVEL;i++) {
			blockNumbers[i]=0;
			previousBlocks[i]=new Block();
		}
		
		//Add the blocks..
		int maxlevel	 = 1;
		int currentlevel = 0;
		previousBlocks[0] = new Block();
		for(int i=0;i<NUMBER_BLOCKS;i++) {
			//Create a new Block..
			Block blk = new Block();
			
			//SetUp
			blk.mBlockHash 			= rand.nextDouble();
			blk.mMaxBlockLevel 		= maxlevel;
			blk.mCurrentBlockLevel 	= currentlevel;
			
			//Add to the Chain
			Blocks.add(blk);
			
			//Add the Block Level Details
			for(int bl=0;bl<=currentlevel;bl++) {
			
				BlockLevel bdata 	 = new BlockLevel();
				bdata.mBlockLevel	 = bl;
				bdata.mBlockNumber	 = blockNumbers[bl];
				bdata.mPreviousBlock = previousBlocks[bl].mBlockHash;
				
				if(bl==0) {
					bdata.mBlockDifficulty 	= 1;
				}else {
//					if(bl == 2) {
//						int hh =0 ;
//					}
//					int start =0;
//					if(blockNumbers[bl] != 0) {
//						//It's not the first..
//						start = previousBlocks[bl].mBlockLevels[0].mBlockNumber+1;
//					}
//					int end   = blk.mBlockLevels[0].mBlockNumber;
//					
//					//Now get all the blocks inbetween
//					ArrayList<Block> repl = getBlocks(Blocks, bl-1, start, end);
//					
//					//Sum
//					double totalpow = 0;
//					for(Block replblk : repl) {
//						totalpow += replblk.mBlockLevels[bl-1].mBlockDifficulty;
//					}
//					bdata.mBlockDifficulty 	= totalpow;
					
					//Simple..
					bdata.mBlockDifficulty 	= Math.pow(2, bl);
				}
				
				blk.mBlockLevels[bl]		= bdata;
			}
			
			//Get the block level and update
			int level = blk.getBlockLevel();
			
			//Increase the current level..
			if(level>=currentlevel) {
				currentlevel++;
				
				if(currentlevel>maxlevel) {
					currentlevel = 0;
					maxlevel++;
				}
			}
	
			if(maxlevel>MAX_BLOCK_LEVEL) {
				maxlevel = MAX_BLOCK_LEVEL;
			}
			
			for(int levup=0;levup<=level;levup++) {
				blockNumbers[levup] += 1;
				previousBlocks[levup] = blk;
			}
		}
		
		//Now print the chains..
		for(int i=0;i<=MAX_BLOCK_LEVEL;i++) {
			MinimaLogger.log("\nBLOCK LEVEL "+i);
			float totalpow = 0;
			int counter=0;
			int lastlevelblock=-1;
			
			for(Block blk : Blocks) {
				if(blk.getBlockLevel()>=i) {
					MinimaLogger.log(counter+") Base Block "+blk.mBlockLevels[0].mBlockNumber);
					counter++;
					blk.printLevels();
					totalpow += blk.mBlockLevels[i].mBlockDifficulty;
					lastlevelblock = blk.mBlockLevels[0].mBlockNumber;
				}
			}
			
			//Have we got the whole chain..
			if(lastlevelblock < NUMBER_BLOCKS - 1) {
				//Cycle one level down..
				for(int extra=i-1;extra>=0;extra--) {
					MinimaLogger.log("**** Level down ****** : "+extra);
					
					for(Block blk : Blocks) {
						if(blk.mBlockLevels[0].mBlockNumber > lastlevelblock) {
							if(blk.checkBlockLevel(extra)) {
								lastlevelblock = blk.mBlockLevels[0].mBlockNumber;
								MinimaLogger.log(counter+") Base Block "+blk.mBlockLevels[0].mBlockNumber);
								totalpow += blk.mBlockLevels[extra].mBlockDifficulty;
								counter++;
								blk.printLevels();
							}
						}
					}	
				}
			}
			
			double blockratio 	= ((double)counter / (double)NUMBER_BLOCKS) * 100;
			double powratio 	= ( totalpow / (double)NUMBER_BLOCKS ) * 100;
			
			MinimaLogger.log("Total TX-POW "+powratio+"%");
			MinimaLogger.log("Size  "+blockratio+"%");
		}
		
	}
	
	public static ArrayList<Block> getBlocks(ArrayList<Block> zAllBlocks, int zLevel, int zStart, int zEnd){
		ArrayList<Block> ret = new ArrayList<>();
		
		boolean startfound = false;
		for(Block blk : zAllBlocks) {
			int bnum = blk.mBlockLevels[0].mBlockNumber;
			
			//is it the start..Level
			if(!startfound) {
				if(bnum == zStart) {
					startfound = true;
				}
			}
			
			//Check it..
			if(startfound) {
				if(blk.checkBlockLevel(zLevel)) {
					ret.add(blk);
				}
			}
			
			//And check for the ending..
			if(bnum == zEnd) {
				//Final.. 
				break;
			}
		}
		
		return ret;
	}
	
}
