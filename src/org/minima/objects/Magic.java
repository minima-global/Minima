package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.math.BigInteger;

import org.minima.GlobalParams;
import org.minima.database.txpowtree.BlockTreeNode;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;

public class Magic implements Streamable {

	/**
	 * How many super levels to average..
	 * 
	 * 12 - means the average taken over 3 months
	 */
	private static final int MAGIC_SUPER_LEVELS = 12;
	
	/**
	 * Default starting values..
	 */
	public static final MiniNumber MIN_TXPOW_SIZE 		= new MiniNumber(32000);
	public static final MiniNumber MIN_TXPOW_TXNS 	 	= new MiniNumber(100);
	public static final MiniNumber MIN_KISSVM_INST		= new MiniNumber(128);
	
	/**
	 * 100,000
	 */
	public static final BigInteger MEGA_VAL 			= Crypto.MAX_VAL.divide(new BigInteger("100000"));	
	public static final MiniData   MIN_TXPOW_WORK		= new MiniData("0x"+MEGA_VAL.toString(16));
	
	/**
	 * The Current MAGIC numbers.. based on a weighted average of the chain..
	 */
	public MiniNumber mCurrentMaxTxPoWSize;
	public MiniNumber mCurrentMaxTxnPerBlock;
	public MiniNumber mCurrentMaxKISSVMInstructions;
	public MiniData   mCurrentMinTxPoWWork;
	
	/**
	 * The Desired MAGIC numbers.. user sets this..
	 */
	public MiniNumber mDesiredMaxTxPoWSize          = MIN_TXPOW_SIZE;
	public MiniNumber mDesiredMaxTxnPerBlock        = MIN_TXPOW_TXNS;
	public MiniNumber mDesiredMaxKISSVMInstructions = MIN_KISSVM_INST;
	public MiniData   mDesiredMinTxPoWWork			= MIN_TXPOW_WORK;
	
	public Magic() {
		mCurrentMaxKISSVMInstructions 	= MIN_KISSVM_INST;
		mCurrentMaxTxPoWSize 			= MIN_TXPOW_SIZE;
		mCurrentMaxTxnPerBlock			= MIN_TXPOW_TXNS;
		mCurrentMinTxPoWWork			= MIN_TXPOW_WORK;
	}

	public JSONObject toJSON() {
		JSONObject magic = new JSONObject();
		
		magic.put("desiredmaxtxpow", mDesiredMaxTxPoWSize.getAsInt());
		magic.put("desiredmaxtxn", mDesiredMaxTxnPerBlock.getAsInt());
		magic.put("desiredmaxkissvm", mDesiredMaxKISSVMInstructions.getAsInt());
		magic.put("desiredmintxpowwork", mDesiredMinTxPoWWork.to0xString());
		
		magic.put("maxtxpow", mCurrentMaxTxPoWSize.getAsInt());
		magic.put("maxtxn", mCurrentMaxTxnPerBlock.getAsInt());
		magic.put("maxkissvm", mCurrentMaxKISSVMInstructions.getAsInt());
		magic.put("mintxpowwork", mCurrentMinTxPoWWork.to0xString());
		
		return magic;
	}
	
	public boolean checkSame(Magic zMagic) {
		boolean x = mCurrentMaxTxPoWSize.isEqual(zMagic.mCurrentMaxTxPoWSize);
		boolean y = mCurrentMaxTxnPerBlock.isEqual(zMagic.mCurrentMaxTxnPerBlock);
		boolean z = mCurrentMaxKISSVMInstructions.isEqual(zMagic.mCurrentMaxKISSVMInstructions);
		boolean w = mCurrentMinTxPoWWork.isEqual(zMagic.mCurrentMinTxPoWWork);
		
		return x && y && z && w;
	}
	
	/**
	 * Get the Magic Parameters
	 */
	public int getMaxTxPoWSize(int zNumTxns) {
		int txnList = zNumTxns * 64;
		int txpow   = mCurrentMaxTxPoWSize.getAsInt();
		return txnList + txpow;
	}
	
	public MiniNumber getMaxNumTxns() {
		return mCurrentMaxTxnPerBlock;
	}
	
	public int getMaxKISSInst() {
		return mCurrentMaxKISSVMInstructions.getAsInt();
	}
	
	public MiniData getMinTxPowWork() {
		return mCurrentMinTxPoWWork;
	}
	
	/**
	 * Calculate the current desired MAX values by taking a 
	 * weighted average of the last MINIMA_CASCADE_LEVEL_NODES blocks at EACH super block level
	 * This is ALWAYS the same and available for every node.. as they all have that many cascade nodes
	 */
	public void calculateCurrentMax(BlockTreeNode mTip) {
		//An array of totals..
		MiniNumber tnum 	= MiniNumber.ZERO;
		int[] numadded 		= new int[MAGIC_SUPER_LEVELS+1];
		
		//Running totals
		Magic ctotal 		= new Magic();
		
		//Set to ZERO
		ctotal.mCurrentMaxKISSVMInstructions 	= MiniNumber.ZERO;
		ctotal.mCurrentMaxTxPoWSize 			= MiniNumber.ZERO;
		ctotal.mCurrentMaxTxnPerBlock			= MiniNumber.ZERO;
		
		//The MinTxPoW Counter
		BigInteger mintxpow = BigInteger.ZERO;
		
		//Set to ZERO..
		for(int l=0;l<=MAGIC_SUPER_LEVELS;l++) {
			numadded[l] = 0;
		}
		
		//Cycle through the chain.. adding to each level accumulator..
		BlockTreeNode current 		= mTip;
		while(current != null) {
			//What Level is this..
			int slevel = current.getSuperBlockLevel();
			
			//Get the Magic
			Magic mag = current.getTxPow().getMagic();
			
			//Run through and add each level!
			for(int num=0;num<=MAGIC_SUPER_LEVELS;num++) {
				if(slevel >= num) {
					if(numadded[num]<GlobalParams.MINIMA_CASCADE_LEVEL_NODES) {
						numadded[num]++;
						MiniNumber multiplier = MiniNumber.TWO.pow(num);
						
						ctotal.mCurrentMaxTxPoWSize = 
								ctotal.mCurrentMaxTxPoWSize.add(mag.mDesiredMaxTxPoWSize.mult(multiplier));
						ctotal.mCurrentMaxTxnPerBlock = 
								ctotal.mCurrentMaxTxnPerBlock.add(mag.mDesiredMaxTxnPerBlock.mult(multiplier));
						ctotal.mCurrentMaxKISSVMInstructions = 
								ctotal.mCurrentMaxKISSVMInstructions.add(mag.mDesiredMaxKISSVMInstructions.mult(multiplier));
						
						//MinTxpow is a little different
						BigInteger magmintxp = mag.mDesiredMinTxPoWWork.getDataValue();
						mintxpow = mintxpow.add(magmintxp.multiply(multiplier.getAsBigInteger()));
						
						//Keep track
						tnum=tnum.add(multiplier);
					}
				}
			}
			
			//Get ther parent..
			current = current.getParent();
		}
		
		//Now do dividion..
		if(tnum.isEqual(MiniNumber.ZERO)) {
			ctotal.mCurrentMaxTxPoWSize 			= MIN_TXPOW_SIZE;
			ctotal.mCurrentMaxTxnPerBlock 			= MIN_TXPOW_TXNS;
			ctotal.mCurrentMaxKISSVMInstructions 	= MIN_KISSVM_INST;
			ctotal.mCurrentMinTxPoWWork 			= MIN_TXPOW_WORK;
		
		}else {
			ctotal.mCurrentMaxTxPoWSize 			= ctotal.mCurrentMaxTxPoWSize.div(tnum).floor();
			ctotal.mCurrentMaxTxnPerBlock 			= ctotal.mCurrentMaxTxnPerBlock.div(tnum).floor();
			ctotal.mCurrentMaxKISSVMInstructions	= ctotal.mCurrentMaxKISSVMInstructions.div(tnum).floor();
		
			//Min Txpow..
			BigInteger mintxpavgval 				= mintxpow.divide(tnum.getAsBigInteger());
			ctotal.mCurrentMinTxPoWWork				= new MiniData("0x"+mintxpavgval.toString(16));
		}
		
		//And set it
		mCurrentMaxTxPoWSize 			= ctotal.mCurrentMaxTxPoWSize;
		mCurrentMaxTxnPerBlock 			= ctotal.mCurrentMaxTxnPerBlock;
		mCurrentMaxKISSVMInstructions 	= ctotal.mCurrentMaxKISSVMInstructions;
		mCurrentMinTxPoWWork			= ctotal.mCurrentMinTxPoWWork;
		
		//Check Within Params..
		if(mCurrentMaxTxPoWSize.isLess(MIN_TXPOW_SIZE)) {
			mCurrentMaxTxPoWSize = MIN_TXPOW_SIZE;
		}
		
		if(mCurrentMaxTxnPerBlock.isLess(MIN_TXPOW_TXNS)) {
			mCurrentMaxTxnPerBlock = MIN_TXPOW_TXNS;
		}
		
		if(mCurrentMaxKISSVMInstructions.isLess(MIN_KISSVM_INST)) {
			mCurrentMaxKISSVMInstructions = MIN_KISSVM_INST;
		}
		
		if(mCurrentMinTxPoWWork.isMore(MIN_TXPOW_WORK)) {
			mCurrentMinTxPoWWork = MIN_TXPOW_WORK;
		}
	}
	
	
	@Override
	public String toString() {
		return toJSON().toString();
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mCurrentMaxTxPoWSize.writeDataStream(zOut);
		mCurrentMaxTxnPerBlock.writeDataStream(zOut);
		mCurrentMaxKISSVMInstructions.writeDataStream(zOut);
		mCurrentMinTxPoWWork.writeDataStream(zOut);
		
		mDesiredMaxTxPoWSize.writeDataStream(zOut);
		mDesiredMaxTxnPerBlock.writeDataStream(zOut);
		mDesiredMaxKISSVMInstructions.writeDataStream(zOut);
		mDesiredMinTxPoWWork.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mCurrentMaxTxPoWSize = MiniNumber.ReadFromStream(zIn);
		mCurrentMaxTxnPerBlock = MiniNumber.ReadFromStream(zIn);
		mCurrentMaxKISSVMInstructions = MiniNumber.ReadFromStream(zIn);
		mCurrentMinTxPoWWork = MiniData.ReadFromStream(zIn);
		
		mDesiredMaxTxPoWSize = MiniNumber.ReadFromStream(zIn);
		mDesiredMaxTxnPerBlock = MiniNumber.ReadFromStream(zIn);
		mDesiredMaxKISSVMInstructions = MiniNumber.ReadFromStream(zIn);
		mDesiredMinTxPoWWork = MiniData.ReadFromStream(zIn);
	}
	
	public static Magic ReadFromStream(DataInputStream zIn) throws IOException {
		Magic mag = new Magic();
		mag.readDataStream(zIn);
		return mag;
	}
}
