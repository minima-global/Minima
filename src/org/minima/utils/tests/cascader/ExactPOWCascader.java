package org.minima.utils.tests.cascader;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Random;

import org.minima.utils.MinimaLogger;

public class ExactPOWCascader {
		
	public static int NUMBER_BLOCKS 		= 99;
	public static int NUMBER_SIMULATIONS 	= 1;
	public static int MAX_BLOCK_LEVEL		= 2;
	
	/**
	 * Simple simulation of a cascading TX-POW chain.
	 * 
	 * A chain requires all blocks to be linked to their previous block. 
	 * 
	 * Is it possible to sum the POW in bits of the chain and create one larger POW value ? 
	 * POST the fact it is not possible. But if you take it into 
	 * account from the start there is a way of combining smaller units of POW.
	 * 
	 * The aim here is to have the total POW in the original chain, without pruning, 
	 * to be equal to the total POW stored in the shorter chain of blocks 
	 * 
	 * @param zArgs
	 */
	public static void main(String[] zArgs) {
		
		//Run the simulation 100 times
		double totalPow=0;
		double totalNum=0;
		for(int i=0;i<NUMBER_SIMULATIONS;i++) {
			MinimaLogger.log("\nRunning Simulation "+i);
			
			runSimulation();
			
			totalPow += POWRatio;
			totalNum += BLKNUMRatio;
		}
		
		MinimaLogger.log("\nFinal Scores");
		MinimaLogger.log("Number of Simulations : "+NUMBER_SIMULATIONS);
		MinimaLogger.log("Number of Blocks : "+NUMBER_BLOCKS);
		MinimaLogger.log("POW Stored : "+(totalPow/(double)NUMBER_SIMULATIONS)*100+"%");
		MinimaLogger.log("SIZE of Cascading Chain : "+(totalNum/(double)NUMBER_SIMULATIONS)*100+"%");
	}
	
	
	public static double POWRatio 		= 0;
	public static double BLKNUMRatio 	= 0;
	
	public static void runSimulation() {
		
		//The simulated HASH function
		Random rand = new Random();
		
		//Hashtable of all the blocks.
		Hashtable<Double, Block> mAllBLocks = new Hashtable<>();
		
		//The chains - we keep both to compare at the end
		ArrayList<Block> Blocks 			= new ArrayList<>();
		ArrayList<Block> BlocksCascading 	= new ArrayList<>();
		int[] mLastLevelBlock				= new int[16];
		
		//Create the Genesis block - only Level 1 allowed
		BlockLevel bdata 		= new BlockLevel();
		bdata.mBlockLevel		= 0;
		bdata.mBlockNumber		= 0;
		bdata.mBlockDifficulty 	= 1;
		bdata.mPreviousBlock	= 0;

		Block mGenesisBlock 		 	= new Block();
		mGenesisBlock.mMaxBlockLevel 	= 0;
		mGenesisBlock.mBlockLevels[0] 	= bdata;
		mGenesisBlock.mBlockHash		= rand.nextDouble();
		
		//Add to the List
		Blocks.add(mGenesisBlock);
		
		//Store the last block
		Block currentblock = mGenesisBlock;
		Block lastBlock;
		
		//Create 100 blocks..
		for(int i=1;i<NUMBER_BLOCKS;i++) {
			//Store
			lastBlock = currentblock;
			
			//What Level waas the last block
			int currentmax 	= lastBlock.mMaxBlockLevel;
			int level 		= lastBlock.getBlockLevel();
			
			//Have we hit the top level
			if(level>=currentmax) {
				level = currentmax+1;
			}else {
				level = currentmax;
			}
			

			if(level>MAX_BLOCK_LEVEL) {
				level= MAX_BLOCK_LEVEL;
			}
			
			//New Block
			currentblock 				= new Block();
			currentblock.mMaxBlockLevel = level;
			currentblock.mBlockHash		= rand.nextDouble();
			
			//Set up the block data
			for(int lev=0;lev<=level;lev++) {
				
				//Create a new BlockLevel
				BlockLevel bl  		= new BlockLevel();
				bl.mBlockLevel 		= lev;
				
				bl.mPreviousBlock 	= 0;
				
				if(lev == 0) {
					bl.mBlockDifficulty = 1.0;
					
				}else {
					//The Block difficulty is equal to the difficulty of 
					//all the blocks this block will replace
					
					//The last block at this level..
					int lastblock = mLastLevelBlock[lev];
					
					//How far back - MUST be at least 2 blocks..
					if(i-lastblock < 2) {
						//Impossible..
						bl.mBlockDifficulty = 0;
						
					}else {
						
						//Get all the blocks..
						ArrayList<Block> betwixt = getAllLevelBlocks(Blocks, lev-1, lastblock, i);
						
						//Sum..
						double totaldiff = 0;
						for(Block bb : betwixt) {
							totaldiff += bb.mBlockLevels[lev-1].mBlockDifficulty;
						}
						
						bl.mBlockDifficulty = totaldiff;
					}
					
					//bl.mBlockDifficulty = Math.pow(10, lev);
				}
				
				//The Blocknumber per level
				if(lev<=currentmax) {
					bl.mBlockNumber     = lastBlock.mBlockLevels[lev].mBlockNumber+1;
				}else{
					bl.mBlockNumber     = 0;
				}
						
				//Add to the current block
				currentblock.mBlockLevels[lev] = bl;
			}
				
			//We know the block level..
			int blevel = currentblock.getBlockLevel();

			//Set the last blocks for each level..
			for(int bl=0;bl<blevel;bl++) {
				mLastLevelBlock[bl] = i;
			}
			
			//Add to the List
			Blocks.add(currentblock);
			
		}
		
		//Now Create the cascading chain
		int counter = 0;
		
		int level 			= 0;
		int levelcounter 	= 0;
		
		MinimaLogger.log("Original Blocks");
		double totalpow 	 = 0;
		boolean limitreached = false;
		int numblock = Blocks.size();
		for(int nblk = 0;nblk<numblock;nblk++) {
			//Get the Block
			Block blk = Blocks.get(nblk);
			
			MinimaLogger.log(counter+") "+blk);
			totalpow+=blk.mBlockLevels[0].mBlockDifficulty;
			counter++;
			
			//This blocks level..
			int blevel = blk.getBlockLevel();
			
			if(blevel >= level) {
				
				//Add to the cascading chain
				BlocksCascading.add(blk);
				
				if(limitreached) {
					//Is it high enough to jump up a level..
					if(blevel>level) {
						level++;
						levelcounter=0;
						limitreached = false;
					}
				}else {
					levelcounter++;
				}
					
				blk.mFinalWeight = blk.mBlockLevels[level].mBlockDifficulty;
				
				if(levelcounter > 10) {
					//Switch to a higher level atg the first opportunity
					limitreached = true;
				}
			}
		}
		MinimaLogger.log("Total POW "+totalpow);
		double tot = totalpow;
		
		MinimaLogger.log("Cascading Chain Blocks");
		counter=0;
		totalpow =0;
		for(Block cblk : BlocksCascading) {
			MinimaLogger.log(counter+") "+cblk);
			totalpow+=cblk.mFinalWeight;
			counter++;
			
		}
		MinimaLogger.log("Total POW "+totalpow);
		
		POWRatio 	= totalpow / tot;
		BLKNUMRatio = (double)BlocksCascading.size() / (double)Blocks.size();
		
		MinimaLogger.log("Ratio Cascading Chain VS Full block chain");
		MinimaLogger.log("Number of blocks : "+(float)(BLKNUMRatio* 100)+"%" );
		MinimaLogger.log("Stored POW       : "+ (float)(POWRatio * 100)+"%" );
		
	}
	
	public static ArrayList<Block> getAllLevelBlocks(ArrayList<Block> zOriginal, int zLevel, int zStart, int zEnd){
		ArrayList<Block> ret = new ArrayList<>();
		
		//Cycle and collect
		for(int bl = zStart;bl<zEnd;bl++) {
			Block blk = zOriginal.get(bl);
			if(blk.getBlockLevel() == zLevel) {
				ret.add(blk);
			}
		}
		
		return ret;
	}
	
}
