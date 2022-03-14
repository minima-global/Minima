package org.minima.system.brains;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
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
	public static final BigInteger MIN_HASHES 		= new BigInteger("750000");
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
		return generateTxPoW(zTransaction, zWitness, null, null);
	}
	
	public static TxPoW generateTxPoW(Transaction zTransaction, Witness zWitness, Transaction zBurnTransaction, Witness zBurnWitness) {
		//Base
		TxPoW txpow = new TxPoW();
		
		//Current top block
		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
		
		//Set the block number
		txpow.setBlockNumber(tip.getTxPoW().getBlockNumber().increment());
				
		//Set the time..
		MiniNumber timenow = txpow.getTimeMilli();
		
		//Check time is in acceptable range.. or will be an invalid block.. 
		boolean wrongtime = false;
		
		TxPoW medianblock = TxPoWGenerator.getMedianTimeBlock(tip, TxPoWChecker.MEDIAN_TIMECHECK_BLOCK).getTxPoW();
		if(timenow.isLess(medianblock.getTimeMilli())) {
			wrongtime = true;
		}else if(timenow.isMore(medianblock.getTimeMilli().add(TxPoWChecker.MAXMILLI_FUTURE))) {
			wrongtime = true;
		}
		
		
		//How much time to add to the median block
		MiniNumber blocksecs 	= MiniNumber.ONE.div(GlobalParams.MINIMA_BLOCK_SPEED);
		MiniNumber half 		= new MiniNumber(TxPoWChecker.MEDIAN_TIMECHECK_BLOCK).div(MiniNumber.TWO); 
		MiniNumber addtime 		= blocksecs.mult(half.add(MiniNumber.ONE)).mult(MiniNumber.THOUSAND);
		
		MinimaLogger.log("");
		MinimaLogger.log("TxPoW block    : "+txpow.getBlockNumber());
		MinimaLogger.log("TxPoW block    : "+new Date(txpow.getTimeMilli().getAsLong()));
		MinimaLogger.log("ESTimare       : "+new Date(medianblock.getTimeMilli().add(addtime).getAsLong()));
		MinimaLogger.log("Median block   : "+new Date(medianblock.getTimeMilli().getAsLong()));
		MinimaLogger.log("DIFF block     : "+txpow.getBlockNumber().sub(medianblock.getBlockNumber()));
		
		MiniNumber difftime 	= txpow.getTimeMilli().sub(medianblock.getTimeMilli());
		MiniNumber diffblocks 	= difftime.div(new MiniNumber(5000));
		MinimaLogger.log("DIFF time      : "+difftime);
		MinimaLogger.log("addsecs        : "+addtime);
		MinimaLogger.log("DIFF blocks    : "+diffblocks);
		
		if(!wrongtime) {
			//Just set the current time
			txpow.setTimeMilli(timenow);
			
		}else {
//			//How much time to add to the median block
//			MiniNumber blocksecs 	= MiniNumber.ONE.div(GlobalParams.MINIMA_BLOCK_SPEED);
//			MiniNumber half 		= new MiniNumber(TxPoWChecker.MEDIAN_TIMECHECK_BLOCK).div(MiniNumber.TWO); 
//			MiniNumber addtime 		= blocksecs.mult(half.add(MiniNumber.ONE)).mult(MiniNumber.THOUSAND);
			
			//Median time + 1 hr..
			txpow.setTimeMilli(medianblock.getTimeMilli().add(addtime));
			MinimaLogger.log("You clock time appears wrong ? Setting acceptable value for TxPoW @ "+txpow.getBlockNumber()+" "+new Date(txpow.getTimeMilli().getAsLong()));
		}
		
		//Set the Transaction..
		txpow.setTransaction(zTransaction);
		txpow.setWitness(zWitness);
		
		//Is there a BURN
		if(zBurnTransaction != null) {
			txpow.setBurnTransaction(zBurnTransaction);
			txpow.setBurnWitness(zBurnWitness);
		}
		
		//Set the correct Magic Numbers..
		Magic txpowmagic = tip.getTxPoW().getMagic().calculateNewCurrent();
		txpow.setMagic(txpowmagic);
		
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
		
		//Set the TXN Difficulty.. currently 1 second work..
		MiniNumber userhashrate = MinimaDB.getDB().getUserDB().getHashRate();
		MiniData minhash 		= calculateDifficultyData(userhashrate);
		if(minhash.isMore(txpowmagic.getMinTxPowWork())) {
			minhash = txpowmagic.getMinTxPowWork();
		}
		txpow.setTxDifficulty(minhash);
		
		//Set the block difficulty - minimum is the TxPoW diff..
		MiniData blkdiff = getBlockDifficulty(tip);
		txpow.setBlockDifficulty(blkdiff);
		
		BigDecimal txpdec 		= new BigDecimal(minhash.getDataValue());
		BigDecimal blockdec 	= new BigDecimal(blkdiff.getDataValue());
		double blockdiffratio 	= txpdec.divide(blockdec, MathContext.DECIMAL32).doubleValue();
		MinimaLogger.log("Diff Ratio : "+blockdiffratio);
		
		//And add the current mempool txpow..
		ArrayList<TxPoW> mempool = MinimaDB.getDB().getTxPoWDB().getAllUnusedTxns();
		
		//Order the mempool txns by BURN..
		Collections.sort(mempool, new Comparator<TxPoW>() {
			@Override
			public int compare(TxPoW o1, TxPoW o2) {
				return o2.getBurn().compareTo(o1.getBurn());
			}
		});
		
		//The final TxPoW transactions put in this TxPoW
		ArrayList<TxPoW> chosentxns = new ArrayList<>();
				
		//A list of the added coins
		ArrayList<String> addedcoins = new ArrayList<>();
		
		//Add the main transaction inputs..
		ArrayList<Coin> inputcoins = zTransaction.getAllInputs();
		for(Coin cc : inputcoins) {
			addedcoins.add(cc.getCoinID().to0xString());
		}
		
		//Burn Coins
		if(zBurnTransaction != null) {
			inputcoins = zBurnTransaction.getAllInputs();
			for(Coin cc : inputcoins) {
				addedcoins.add(cc.getCoinID().to0xString());
			}
		}
	
		//Check them all..
		int totaladded = 0;
		for(TxPoW memtxp : mempool) {
			
			//Is it a transaction
			if(!memtxp.isTransaction()) {
				continue;
			}
			
			//Start off assuming it's valid
			boolean valid = true;
			try {
				
				//Check CoinIDs not added already..
				ArrayList<Coin> inputs = memtxp.getTransaction().getAllInputs();
				for(Coin cc : inputs) {
					if(addedcoins.contains(cc.getCoinID().to0xString())) {
						//Coin already added in previous TxPoW
						continue;
					}
				}
				
				//Check against the Magic Numbers
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
				}
				
			}catch(Exception exc) {
				MinimaLogger.log("ERROR Checking TxPoW "+memtxp.getTxPoWID()+" "+exc.toString());
				valid = false;
			}
			
			//Was it valid
			if(!valid) {
				//Invalid TxPoW - remove from mempool
				MinimaLogger.log("Invalid TxPoW in mempool.. removing.. "+memtxp.getTxPoWID());
				MinimaDB.getDB().getTxPoWDB().removeMemPoolTxPoW(memtxp.getTxPoWID());
			}
			
			//Max allowed..
			if(totaladded >= txpowmagic.getMaxNumTxns().getAsInt()) {
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
	
	public static MiniData getBlockDifficulty(TxPoWTreeNode zParent) {
		
		//Are we just starting out..
		if(zParent.getBlockNumber().isLess(new MiniNumber(GlobalParams.MEDIAN_BLOCK_CALC))) {
			return MIN_TXPOWDIFF;
		}
		
		//Start from the parent..
		TxPoWTreeNode startblock = zParent;
		
		//Where to..
		TxPoWTreeNode endblock 	= zParent.getParent(GlobalParams.MINIMA_BLOCKS_SPEED_CALC.getAsInt());
		
		//Now use the Median Times..
		startblock 	= getMedianTimeBlock(startblock, GlobalParams.MEDIAN_BLOCK_CALC);
		endblock 	= getMedianTimeBlock(endblock, GlobalParams.MEDIAN_BLOCK_CALC);
		MiniNumber blockdiff = startblock.getBlockNumber().sub(endblock.getBlockNumber()); 
		
		//Get current speed
		MiniNumber speed 				= getChainSpeed(startblock, blockdiff);
		MiniNumber speedratio 			= GlobalParams.MINIMA_BLOCK_SPEED.div(speed);
		
		//Get average difficulty over that period
		BigInteger averagedifficulty 	= getAverageDifficulty(startblock, blockdiff);
		BigDecimal averagedifficultydec	= new BigDecimal(averagedifficulty);
		
		//Recalculate..
		BigDecimal newdifficultydec = averagedifficultydec.multiply(speedratio.getAsBigDecimal());  
		BigInteger newdifficulty	= newdifficultydec.toBigInteger();
		
		//MUST be more difficult (lower) than the MIN TxPoW..
		if(newdifficulty.compareTo(MIN_TXPOW_VAL)>0) {
			newdifficulty = MIN_TXPOW_VAL;
		}
		
		//THIS SHOULD NEVER HAPPEN
		if(newdifficulty.compareTo(BigInteger.ZERO)<0) {
			MinimaLogger.log("SERIOUS ERROR : NEGATIVE DIFFICULTY!");
			MinimaLogger.log("speed         : "+speed);
			MinimaLogger.log("speedratio    : "+speedratio);
			MinimaLogger.log("newdifficulty :"+newdifficulty.toString());
			
			//Set the Old value..
			newdifficulty = startblock.getTxPoW().getBlockDifficulty().getDataValue();
		}
		
		return new MiniData(newdifficulty);
	
	}
	
	public static MiniNumber getChainSpeed(TxPoWTreeNode zTopBlock, MiniNumber zBlocksBack) {
		
		//Get the past block
		TxPoWTreeNode pastblock = zTopBlock.getParent(zBlocksBack.getAsInt());
		
		MiniNumber blockpast	= pastblock.getTxPoW().getBlockNumber();
		MiniNumber timepast 	= pastblock.getTxPoW().getTimeMilli();
		
		MiniNumber blocknow		= zTopBlock.getTxPoW().getBlockNumber();
		MiniNumber timenow 		= zTopBlock.getTxPoW().getTimeMilli();
		
		MiniNumber blockdiff 	= blocknow.sub(blockpast);
		MiniNumber timediff 	= timenow.sub(timepast);
		
		MiniNumber speedmilli 	= blockdiff.div(timediff);
		MiniNumber speedsecs 	= speedmilli.mult(MiniNumber.THOUSAND);
		
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
	public static TxPoWTreeNode getMedianTimeBlock(TxPoWTreeNode zStartBlock, int zBlocksBack) {
		
		//The block we start checking from
		TxPoWTreeNode current = zStartBlock;
		
		//Create a list of blocks..
		ArrayList<TxPoWTreeNode> allblocks = new ArrayList<>();
		
		int counter=0;
		while(counter<zBlocksBack && current!=null) {
			
			//Add to our list
			allblocks.add(current);
			
			//Move back up the tree
			current = current.getParent();
			counter++;
		}
		
		//Now sort them.. by time milli
		Collections.sort(allblocks, new Comparator<TxPoWTreeNode>() {
			@Override
			public int compare(TxPoWTreeNode o1, TxPoWTreeNode o2) {
				return o1.getTxPoW().getTimeMilli().compareTo(o2.getTxPoW().getTimeMilli());
			}
		});
		
		//Now pick the middle one
		int middle 	= allblocks.size()/2;
		
		//Return the middle one!
		return allblocks.get(middle);
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
	
	public static void main(String[] zArgs) {
		
		ArrayList<MiniNumber> nums = new ArrayList<>();
		nums.add(MiniNumber.ZERO);
		nums.add(MiniNumber.ONE);
		nums.add(MiniNumber.TWO);
		
		Collections.sort(nums, new Comparator<MiniNumber>() {
			@Override
			public int compare(MiniNumber o1, MiniNumber o2) {
				return o2.compareTo(o1);
			}
		});
		
		System.out.println(nums.toString());
	}
}
