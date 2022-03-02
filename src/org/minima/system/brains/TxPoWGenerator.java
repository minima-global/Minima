package org.minima.system.brains;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;

import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMRData;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.objects.Coin;
import org.minima.objects.Magic;
import org.minima.objects.Transaction;
import org.minima.objects.TxBlock;
import org.minima.objects.TxPoW;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.params.GlobalParams;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;

public class TxPoWGenerator {
	
	/**
	 * For Now - Hard set the Min TxPoW Difficulty
	 */
	public static final BigInteger MIN_HASHES 		= new BigInteger("10000");
	public static final BigInteger MIN_TXPOW_VAL 	= Crypto.MAX_VAL.divide(MIN_HASHES);
	public static final MiniData MIN_TXPOWDIFF 		= new MiniData(MIN_TXPOW_VAL);
	
	/**
	 * Calculate a Difficulty Hash for a given hash number
	 */
	public static MiniData calculateDifficultyData(MiniNumber zHashes) {
		return new MiniData(Crypto.MAX_VAL.divide(zHashes.getAsBigInteger()));
	}
	
	/**
	 * Generate a complete TxPoW
	 */
	public static TxPoW generateTxPoW(Transaction zTransaction, Witness zWitness) {
		//Base
		TxPoW txpow = new TxPoW();
		
		//Current top block
		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
		
		//Set the time..
		txpow.setTimeMilli(new MiniNumber(System.currentTimeMillis()));
		
		//Set the Transaction..
		txpow.setTransaction(zTransaction);
		txpow.setWitness(zWitness);
		
		//Set the correct Magic Numbers..
		Magic txpowmagic = tip.getTxPoW().getMagic().calculateNewCurrent();
		txpow.setMagic(txpowmagic);
		
		//Set the TXN Difficulty..
		MiniNumber userhashrate = MinimaDB.getDB().getUserDB().getHashRate();
		MiniData minhash 		= calculateDifficultyData(userhashrate);
		if(minhash.isMore(txpowmagic.getMinTxPowWork())) {
			minhash = txpowmagic.getMinTxPowWork();
		}
		txpow.setTxDifficulty(minhash);
		
		//Set the details..
		txpow.setBlockNumber(tip.getTxPoW().getBlockNumber().increment());
		
		//Set the parents..
		for(int i=0;i<GlobalParams.MINIMA_CASCADE_LEVELS;i++) {
			txpow.setSuperParent(i, tip.getTxPoW().getSuperParent(i));
		}

		//And now set the correct SBL given the last block
		int sbl = tip.getTxPoW().getSuperLevel();
				
		//All levels below this now point to the last block..
		MiniData tiptxid = tip.getTxPoW().getTxPoWIDData();
		for(int i=sbl;i>=0;i--) {
			txpow.setSuperParent(i, tiptxid);
		}
		
		/**
		 * Calculate the current speed and block difficulty
		 */
		MiniNumber topblock = tip.getTxPoW().getBlockNumber();
		
		//Get the media Tip block..
		TxPoW tipblock 	 	= getMedianTimeBlock(tip, 32);
		MiniNumber topblock = tip.getTxPoW().getBlockNumber();
		
		
		//First couple of blocks 
		if(topblock.isLessEqual(MiniNumber.TWO)) {
			txpow.setBlockDifficulty(MIN_TXPOWDIFF);
		}else {
			MiniNumber blocksback = GlobalParams.MINIMA_BLOCKS_SPEED_CALC;
			if(topblock.isLessEqual(blocksback)) {
				blocksback = topblock.decrement();
			}
			
			//Find a block to use as the base for calculations
			TxPoWTreeNode current = tip;
			int counter	=0;
			int max 	=blocksback.getAsInt(); 
			while(counter<max && current.getParent()!=null) {
				current = current.getParent();
				counter++;
			}
			
			//Now we have a block in the past.. get the median time value block around it
			TxPoW baseblock 	 = getMedianTimeBlock(current, 32);
			MiniNumber blockdiff = topblock.sub(baseblock.getBlockNumber()); 
			
			//Get current speed
			MiniNumber speed 				= getChainSpeed(tip, blockdiff);
			MiniNumber speedratio 			= GlobalParams.MINIMA_BLOCK_SPEED.div(speed);
			
			//Get average difficulty over that period
			BigInteger averagedifficulty 	= getAverageDifficulty(tip, blockdiff);
			BigDecimal averagedifficultydec	= new BigDecimal(averagedifficulty);
			
			//Recalculate..
			BigDecimal newdifficultydec = averagedifficultydec.multiply(speedratio.getAsBigDecimal());  
			BigInteger newdifficulty	= newdifficultydec.toBigInteger();
			
			//MUST be more difficult (lower) than the MIN TxPoW..
			if(newdifficulty.compareTo(MIN_TXPOW_VAL)>0) {
				newdifficulty = MIN_TXPOW_VAL;
			}
			
			if(newdifficulty.compareTo(BigInteger.ZERO)<0) {
				MinimaLogger.log("SERIOUS ERROR : NEGATIVE DIFFICULTY!");
				MinimaLogger.log("speed         : "+speed);
				MinimaLogger.log("speedratio    : "+speedratio);
				MinimaLogger.log("newdifficulty :"+newdifficulty.toString());
				
				//Set the Old value..
				newdifficulty = tip.getTxPoW().getBlockDifficulty().getDataValue();
			}
			
			txpow.setBlockDifficulty(new MiniData(newdifficulty));
		}
		
		//And add the current mempool txpow..
		ArrayList<TxPoW> mempool = MinimaDB.getDB().getTxPoWDB().getAllUnusedTxns();
		
		//The final TxPoW transactions put in this TxPoW
		ArrayList<TxPoW> chosentxns = new ArrayList<>();
		
		//Order the mempool by BURN..
		//..
		
		//A list of the added coins
		ArrayList<String> addedcoins = new ArrayList<>();
		
		//Add the main transaction inputs..
		ArrayList<Coin> inputcoins = txpow.getTransaction().getAllInputs();
		for(Coin cc : inputcoins) {
			addedcoins.add(cc.getCoinID().to0xString());
		}
		
		//Check them all..
		int totaladded = 0;
		for(TxPoW memtxp : mempool) {
			
			//Is it a transaction
			if(!memtxp.isTransaction()) {
				continue;
			}
			
			try {
				
				//Check CoinIDs not added already..
				ArrayList<Coin> inputs = memtxp.getTransaction().getAllInputs();
				for(Coin cc : inputs) {
					if(addedcoins.contains(cc.getCoinID().to0xString())) {
						//Coin already added in previous TxPoW
						continue;
					}
				}
				
				boolean valid = true;
				if(memtxp.getSizeinBytesWithoutBlockTxns() > txpowmagic.getMaxTxPoWSize().getAsLong()) {
					MinimaLogger.log("Memepool txn too big.. "+memtxp.getTxPoWID());
					valid = false;
				}else if(memtxp.getTxnDifficulty().isMore(txpowmagic.getMinTxPowWork())) {
					MinimaLogger.log("Memepool txn TxPoW too low.. "+memtxp.getTxPoWID());
					valid = false;
				}
				
				//Check if Valid!
				if(valid && TxPoWChecker.checkTxPoWSimple(tip.getMMR(), memtxp, txpow)) {
					//Add to our list
					chosentxns.add(memtxp);
					
					//Add to this TxPoW
					txpow.addBlockTxPOW(memtxp.getTxPoWIDData());
					
					//One more to the total..
					totaladded++;
					
					//Add all the input coins
					ArrayList<Coin> memtxpinputcoins = memtxp.getTransaction().getAllInputs();
					for(Coin cc : memtxpinputcoins) {
						addedcoins.add(cc.getCoinID().to0xString());
					}
					
				}else {
					
					//Invalid TxPoW - remove from mempool
					MinimaLogger.log("Invalid TxPoW in mempool.. removing.. "+memtxp.getTxPoWID());
					MinimaDB.getDB().getTxPoWDB().removeMemPoolTxPoW(memtxp.getTxPoWID());
				}
				
			}catch(Exception exc) {
				MinimaLogger.log("ERROR Checking TxPoW "+memtxp.getTxPoWID()+" "+exc.toString());
			}
			
			//Max allowed..
			if(totaladded > txpowmagic.getMaxNumTxns().getAsInt()) {
				break;
			}
		}
		
		//Calculate the TransactionID - needed for CoinID and MMR..
		txpow.calculateTransactionID();
		
		//Construct the MMR
		TxBlock txblock 	= new TxBlock(tip.getMMR(), txpow, chosentxns);
		TxPoWTreeNode node 	= new TxPoWTreeNode(txblock, false);
		
		//Get the MMR root data
		MMRData root = node.getMMR().getRoot();
		txpow.setMMRRoot(root.getData());
		txpow.setMMRTotal(root.getValue());
		
		return txpow;
	}
	
	
	public static MiniNumber getChainSpeed(TxPoWTreeNode zTopBlock, MiniNumber zBlocksBack) {
		
		//Which block are we looking for..
		MiniNumber block = zTopBlock.getTxPoW().getBlockNumber().sub(zBlocksBack);
		
		//Get the past block - initially there may be less than that available
		TxPoWTreeNode pastblock = zTopBlock.getPastNode(block);
		if(pastblock == null) {
			//too soon..
			MinimaLogger.log("SPEED TOO SOON "+zTopBlock.getTxPoW().getBlockNumber()+" "+zBlocksBack);
			return MiniNumber.ONE;
		}
		
		MiniNumber blockpast	= pastblock.getTxPoW().getBlockNumber();
		MiniNumber timepast 	= pastblock.getTxPoW().getTimeMilli();
		
		MiniNumber blocknow		= zTopBlock.getTxPoW().getBlockNumber();
		MiniNumber timenow 		= zTopBlock.getTxPoW().getTimeMilli();
		
		MiniNumber blockdiff 	= blocknow.sub(blockpast);
		MiniNumber timediff 	= timenow.sub(timepast);
		
		MiniNumber speedmilli 	= blockdiff.div(timediff);
		MiniNumber speedsecs 	= speedmilli.mult(MiniNumber.THOUSAND);
		
		if(speedsecs.isLessEqual(MiniNumber.ZERO)) {
			MinimaLogger.log("SERIOUS ERROR NEGATIVE SPEED AS PAST BLOCK AHEAD OF CURRENT TIME!");
			MinimaLogger.log(blockpast+" "+new Date(timepast.getAsLong()).toString()+" "+blocknow+" "+new Date(timenow.getAsLong()).toString());
			MinimaLogger.log("PAST    : "+blockpast+" "+pastblock.getTxPoW().getTxPoWID()+" "+timepast.getAsLong());
			MinimaLogger.log("CURRENT : "+blocknow+" "+zTopBlock.getTxPoW().getTxPoWID()+" "+timenow.getAsLong());
			return GlobalParams.MINIMA_BLOCK_SPEED;
		}
		
		return speedsecs;
	}
	
	private static BigInteger getAverageDifficulty(TxPoWTreeNode zTopBlock, MiniNumber zBlocksBack) {
		BigInteger total 	= BigInteger.ZERO;
		int totalblock 		= zBlocksBack.getAsInt();
		
		TxPoWTreeNode current 	= zTopBlock;
		int counter 			= 0;
		while(counter<totalblock) {
			MiniData difficulty = current.getTxPoW().getBlockDifficulty();
			BigInteger diffval 	= difficulty.getDataValue();
			
			//Add to the total..
			total = total.add(diffval);
			
			//get the parent..
			current = current.getParent();
			counter++;
		}
		
		//Now do the div..
		BigInteger avg = total.divide(new BigInteger(Integer.toString(counter)));
		
		return avg;
	}
	
	/**
	 * Get the Median Block based on milli time..
	 */
	public static TxPoW getMedianTimeBlock(TxPoWTreeNode zStartBlock, int zBlocksBack) {
		
		//The block we start checking from
		TxPoWTreeNode current = zStartBlock;
		
		//Create a list of blocks..
		ArrayList<TxPoW> allblocks = new ArrayList<>();
		
		int counter=0;
		while(counter<zBlocksBack && current!=null) {
			
			//Add to our list
			allblocks.add(current.getTxPoW());
			
			//Move back up the tree
			current = current.getParent();
			counter++;
		}
		
		//Now sort them.. by time milli
		Collections.sort(allblocks, new Comparator<TxPoW>() {
			@Override
			public int compare(TxPoW o1, TxPoW o2) {
				return o1.getTimeMilli().compareTo(o2.getTimeMilli());
			}
		});
		
		//Now pick the middle one
		int size 	= allblocks.size();
		int middle 	= size/2;
		
		//Middle..
		TxPoW median = allblocks.get(middle);
		
//		String timenow 		= new Date(zStartBlock.getTxPoW().getTimeMilli().getAsLong()).toString();
//		String timemedian 	= new Date(median.getTimeMilli().getAsLong()).toString();
//		MinimaLogger.log("MEDIAN TIME @ "+zStartBlock.getTxPoW().getBlockNumber()+" MEDIAN:"+median.getBlockNumber()+" "+counter+" "+middle);
		
		//Return the middle one!
		return median;
	}
	
	public static void precomputeTransactionCoinID(Transaction zTransaction) {
		
		//Get the inputs.. 
		ArrayList<Coin> inputs = zTransaction.getAllInputs();
		
		//Are there any..
		if(inputs.size() == 0) {
			return;
		}
		
		//Get the first coin..
		Coin firstcoin = inputs.get(0);
		
		//Is it an ELTOO input
		boolean eltoo = false; 
		if(firstcoin.getCoinID().isEqual(Coin.COINID_ELTOO)) {
			eltoo = true;
		}
		
		//The base modifier
		MiniData basecoinid = Crypto.getInstance().hashAllObjects(
										firstcoin.getCoinID(),
										firstcoin.getAddress(),
										firstcoin.getAmount(),
										firstcoin.getTokenID());
		
		//Now cycle..
		ArrayList<Coin> outputs = zTransaction.getAllOutputs();
		int num=0;
		for(Coin output : outputs) {
			
			//Calculate the CoinID..
			if(eltoo) {
				
				//Normal
				output.resetCoinID(Coin.COINID_OUTPUT);
				
			}else {
				
				//The CoinID
				MiniData coinid = zTransaction.calculateCoinID(basecoinid, num);
				output.resetCoinID(coinid);
			}
			
			num++;
		}
	}
}
