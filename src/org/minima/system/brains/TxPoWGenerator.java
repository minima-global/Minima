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
	 * TESTER hash difficulty
	 */
	public static final MiniData MIN_DIFFICULTY = new MiniData(
					"0xFFFFFFFFFFFFFFFFFFFF"+
					  "FFFFFFFFFFFFFFFFFFFF"+
					  "FFFFFFFFFFFFFFFFFFFF"+
					  "FFFF");
	
	/**
	 * For Now - Hard set the Min TxPoW Difficulty
	 */
	public static final BigInteger MIN_HASHES 		= new BigInteger("10000");
	public static final BigInteger MIN_TXPOW_VAL 	= Crypto.MAX_VAL.divide(MIN_HASHES);
	public static final MiniData MIN_TXPOWDIFF 		= new MiniData(MIN_TXPOW_VAL);
	
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
		
		//Set the TXN Difficulty..
		txpow.setTxDifficulty(MIN_TXPOWDIFF);
		
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
		
		//First couple of blocks 
		if(topblock.isLessEqual(MiniNumber.TWO)) {
			txpow.setBlockDifficulty(MIN_TXPOWDIFF);
		}else {
			MiniNumber blocksback = GlobalParams.MINIMA_BLOCKS_SPEED_CALC;
			if(topblock.isLessEqual(blocksback)) {
				blocksback = topblock.decrement();
			}
			
			//Get current speed
			MiniNumber speed 		= getChainSpeed(tip, blocksback);
			MiniNumber speedratio 	= GlobalParams.MINIMA_BLOCK_SPEED.div(speed);
			
			//Get average difficulty over that period
			BigInteger averagedifficulty 	= getAverageDifficulty(tip, blocksback);
			BigDecimal averagedifficultydec	= new BigDecimal(averagedifficulty);
			
			//Recalculate..
			BigDecimal newdifficultydec = averagedifficultydec.multiply(speedratio.getAsBigDecimal());  
			BigInteger newdifficulty	= newdifficultydec.toBigInteger();
			
			//MUST be more than the MIN TxPoW..
			if(newdifficulty.compareTo(MIN_TXPOW_VAL)>0) {
				newdifficulty = MIN_TXPOW_VAL;
			}
			
			if(newdifficulty.compareTo(BigInteger.ZERO)<0) {
				MinimaLogger.log("SERIOUS ERROR : NEGATIVE DIFFICULTY!");
				MinimaLogger.log("speed         : "+speed);
				MinimaLogger.log("speedratio    : "+speedratio);
				MinimaLogger.log("newdifficulty :"+newdifficulty.toString());
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
			
			try {
				
				//Check CoinIDs not added already..
				ArrayList<Coin> inputs = memtxp.getTransaction().getAllInputs();
				for(Coin cc : inputs) {
					if(addedcoins.contains(cc.getCoinID().to0xString())) {
						//Coin already added in previous TxPoW
						continue;
					}
				}
				
				//Check if Valid!
				if(TxPoWChecker.checkTxPoWSimple(tip.getMMR(), memtxp, txpow.getBlockNumber())) {
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
				MinimaLogger.log("ERROR Checking TxPoW "+memtxp.getTxPoWID());
			}
			
			//Max allowed.. 1 txn/s - for now..
			if(totaladded > 50) {
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
	 * Get the Median time of the last 128 blocks..
	 */
	public static MiniNumber getMedianTime(TxPoWTreeNode zTopBlock) {
		
		//Create a list of times..
		ArrayList<MiniNumber> alltimes = new ArrayList<>();
		
		TxPoWTreeNode current = zTopBlock;
		int counter=0;
		while(counter<128 && current!=null) {
			
			//Add to our list
			alltimes.add(current.getTxPoW().getTimeMilli());
			
			//Move back up the tree
			current = current.getParent();
			counter++;
		}
		
		//Now sort them..
		Collections.sort(alltimes, new Comparator<MiniNumber>() {
			@Override
			public int compare(MiniNumber o1, MiniNumber o2) {
				return o1.compareTo(o2);
			}
		});
		
		//Now pick the middle one
		int size = alltimes.size();
		
		//Middle..
		MiniNumber median = alltimes.get(size/2);
		
//		String timenow 		= new Date(zTopBlock.getTxPoW().getTimeMilli().getAsLong()).toString();
//		String timemedian 	= new Date(median.getAsLong()).toString();
//		MinimaLogger.log("MEDIAN TIME @ "+timenow+" MEDIAN:"+timemedian+" "+counter);
		
		//Return the middle one!
		return median;
	}
}
