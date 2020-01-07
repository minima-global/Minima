package org.minima.utils.tests.cchain;

import java.util.Random;

import org.minima.utils.MinimaLogger;

public class CChainSim {

	public static int NUMBER_BLOCKS 		= 1000;
	public static int NUMBER_SIMULATIONS 	= 1;
	
	public static void main(String[] zArgs) {
		if(zArgs.length>0) {
			NUMBER_BLOCKS = Integer.parseInt(zArgs[0]);
		}
		
		//Need a hash generator
		Random hash = new Random();
		
		//The main chain
		CChain mainChain = new CChain();
		
//		//First add the Genesis Block
//		CChainBlock genesisBlock = new CChainBlock();
//		genesisBlock.mBlockHash			= hash.nextDouble();
//		genesisBlock.mBaseBlockNumber 	= 0;
//		genesisBlock.mBasePrevBlockHash = 0;
//		genesisBlock.mCurrentBlockLevel = 0;
//		
//		//You ALWAYS add a block level 0
//		CChainLevel cclevel  			= new CChainLevel();
//		cclevel.mBlockLevel  			= 0;
//		cclevel.mBlockNumber 			= 0;
//		cclevel.mPreviousBlock 			= 0;
//		cclevel.mBlockDifficulty 		= 1.0;
//		genesisBlock.mBlockLevels[0]	= cclevel;
//		
//		//Set it..
//		mainChain.addBlock(genesisBlock);
		
		//Lets add some blocks
//		int baseBlockNumber = genesisBlock.mBaseBlockNumber+1;
//		double basePrevHash = genesisBlock.mBlockHash;
//		
		int baseBlockNumber = 0;
		double basePrevHash = 0;
		
		for(int bloop=0;bloop<NUMBER_BLOCKS;bloop++) {
			///Create a new Block
			CChainBlock newBlock = new CChainBlock();
			
			newBlock.mBlockHash		  	= hash.nextDouble();
//			newBlock.mBlockHash		  	= 0.3;
			
			newBlock.mBaseBlockNumber 	= baseBlockNumber;
			newBlock.mBasePrevBlockHash	= basePrevHash;
			newBlock.mCurrentBlockLevel = 0;
			
			//Now add the Block Difficulty Levels
			//You ALWAYS add a block level 0
			CChainLevel cclevel			= new CChainLevel();
			cclevel.mBlockLevel  		= 0;
			cclevel.mBlockNumber 		= baseBlockNumber;
			cclevel.mPreviousBlock 		= basePrevHash;
			cclevel.mBlockDifficulty 	= 1.0;
			newBlock.mBlockLevels[0]	= cclevel;
			
			//Now Can we add other Levels..
			int maxlevel=0;
			boolean[] levels = null;
			
			//Is it the Genesis Block
			if(baseBlockNumber==0) {
				levels = new boolean[CChainBlock.MAX_LEVELS];
				levels[0] = false;
			}else {
				levels = mainChain.getLevels();
			}
			
			for(int check=0;check<levels.length;check++) {
				if(levels[check]) {
					maxlevel=check+1;
				}
			}
			
//			if(maxlevel>1) {
//				maxlevel = 1;
//			}
			if(maxlevel>CChainBlock.MAX_LEVELS-1) {
				maxlevel = CChainBlock.MAX_LEVELS-1;
			}
			
			//Add it to the chain
			mainChain.addBlock(newBlock);
			
			//now add.. starting from level 1 (0 already added)
			for(int lev=1;lev<=maxlevel;lev++) {
				cclevel  					= new CChainLevel();
				cclevel.mBlockLevel  		= lev;
				cclevel.mBlockNumber 		= newBlock.mBaseBlockNumber;
				cclevel.mPreviousBlock 		= newBlock.mBasePrevBlockHash;
				
				//If a lower block exists
				if(levels[lev-1]) {
					//Calculate the total diff..
//					double totaldiff = CChain.getTotalLevelDiff(newBlock,lev);
//					cclevel.mBlockDifficulty = totaldiff;
					
					cclevel.mBlockDifficulty 	= Math.pow(2, lev);
				}else {
					//It's impossible
					cclevel.mBlockDifficulty 	= 0;
				}
				
				//Set it
				newBlock.mBlockLevels[lev]	= cclevel;
				
				//Change the current block level..
				newBlock.mCurrentBlockLevel = lev;
			}
			
			//Add it to the Chain
//			System.out.print("\033[H\033[2J");  
//		    System.out.flush();  
		    
			MinimaLogger.log("********  NEW BLOCK ADDED!! ***\n"+newBlock);
//			SimpleLogger.log("\n********  NEW BLOCK ADDED!! *** "+newBlock.getFinalLevel());
			MinimaLogger.log("OLD  : "+mainChain.toLevelChain());
			
			//Create a cascading chain
			Cascader casc = new Cascader(mainChain);
			
			//And Switch
			mainChain = casc.getAsCChain();
			
			MinimaLogger.log("NEW  : "+mainChain.toLevelChain());
			
			double powperc = ((double)mainChain.getTotalPOW() / (NUMBER_BLOCKS)) * 100;
			MinimaLogger.log("Total POW : "+powperc+"%");
			
			double perc = ((double)mainChain.getSize() / (NUMBER_BLOCKS)) * 100;
			MinimaLogger.log("Size : "+perc+"% [ "+mainChain.getSize()+" / "+NUMBER_BLOCKS+" ]");
			
			//Base values
			baseBlockNumber++;
			basePrevHash = newBlock.mBlockHash;
		}
	}
}
