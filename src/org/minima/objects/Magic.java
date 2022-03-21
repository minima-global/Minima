package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.math.BigInteger;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Crypto;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;

/**
 * These Numbers define the capacity of the Minima network
 * 
 * @author spartacusrex
 *
 */
public class Magic implements Streamable {

	/**
	 * Used to calculate the weighted averages
	 */
	private static final MiniNumber CALC_WEIGHTED		= new MiniNumber(16383);
	private static final MiniNumber CALC_TOTAL 			= new MiniNumber(16384);
	
	/**
	 * These are HARD limits that can NEVER Change
	 */
	private static final MiniNumber MINMAX_TXPOW_SIZE 			= new MiniNumber(64*1024);
	private static final MiniNumber MINMAX_KISSVM_OPERATIONS 	= new MiniNumber(1024);
	private static final MiniNumber MINMAX_TXPOW_TXNS			= new MiniNumber(256);
	
	/**
	 * Minimum acceptable PoW per TxPoW - Also a HARD limit
	 * 
	 * 0.1 MHash is the minimum..
	 */
	public static final MiniNumber MIN_HASHES 		= new MiniNumber(100000);
	public static final BigInteger MIN_TXPOW_VAL 	= Crypto.MAX_VAL.divide(MIN_HASHES.getAsBigInteger());
	public static final MiniData MIN_TXPOW_WORK 	= new MiniData(MIN_TXPOW_VAL);
	
	/**
	 * Default Maximum size of a TxPoW unit.. Can change
	 */
	private static final MiniNumber DEFAULT_TXPOW_SIZE 	= new MiniNumber(64*1024);
	
	/**
	 * Default Maximum Number of executed KISSVM Operations
	 */
	private static final MiniNumber DEFAULT_KISSVM_OPERATIONS 	= new MiniNumber(1024);
	
	/**
	 * Default Maximum number of Txns per block
	 */
	private static final MiniNumber DEFAULT_TXPOW_TXNS	= new MiniNumber(256);
	
		
	/**
	 * The Current MAGIC numbers.. based on a weighted average of the chain..
	 * 
	 * This is ( 16383*the last current values + 1*Desired value ) / 16384
	 */
	public MiniNumber mCurrentMaxTxPoWSize;
	public MiniNumber mCurrentMaxKISSVMOps;
	public MiniNumber mCurrentMaxTxnPerBlock;
	public MiniData   mCurrentMinTxPoWWork;
	
	public MiniNumber mDesiredMaxTxPoWSize;
	public MiniNumber mDesiredMaxKISSVMOps;
	public MiniNumber mDesiredMaxTxnPerBlock;
	public MiniData   mDesiredMinTxPoWWork;
	
	public Magic() {
		mCurrentMaxTxPoWSize			= DEFAULT_TXPOW_SIZE;
		mCurrentMaxKISSVMOps			= DEFAULT_KISSVM_OPERATIONS;
		mCurrentMaxTxnPerBlock			= DEFAULT_TXPOW_TXNS;
		mCurrentMinTxPoWWork			= MIN_TXPOW_WORK;
		
		mDesiredMaxTxPoWSize			= DEFAULT_TXPOW_SIZE;
		mDesiredMaxKISSVMOps			= DEFAULT_KISSVM_OPERATIONS;
		mDesiredMaxTxnPerBlock        	= DEFAULT_TXPOW_TXNS;
		mDesiredMinTxPoWWork			= MIN_TXPOW_WORK;
	}

	public JSONObject toJSON() {
		JSONObject magic = new JSONObject();
		
		magic.put("currentmaxtxpowsize", mCurrentMaxTxPoWSize.toString());
		magic.put("currentmaxkissvmops", mCurrentMaxKISSVMOps.toString());
		magic.put("currentmaxtxn", mCurrentMaxTxnPerBlock.toString());
		magic.put("currentmintxpowwork", mCurrentMinTxPoWWork.to0xString());
		
		magic.put("desiredmaxtxpowsize", mDesiredMaxTxPoWSize.toString());
		magic.put("desiredmaxkissvmops", mDesiredMaxKISSVMOps.toString());
		magic.put("desiredmaxtxn", mDesiredMaxTxnPerBlock.toString());
		magic.put("desiredmintxpowwork", mDesiredMinTxPoWWork.to0xString());
		
		return magic;
	}
	
	public boolean checkSame(Magic zMagic) {
		boolean w = mCurrentMaxTxPoWSize.isEqual(zMagic.mCurrentMaxTxPoWSize);
		boolean x = mCurrentMaxKISSVMOps.isEqual(zMagic.mCurrentMaxKISSVMOps);
		boolean y = mCurrentMaxTxnPerBlock.isEqual(zMagic.mCurrentMaxTxnPerBlock);
		boolean z = mCurrentMinTxPoWWork.isEqual(zMagic.mCurrentMinTxPoWWork);
		
		return w && x && y && z;
	}
	
	/**
	 * Get the Magic Parameters
	 */
	public MiniNumber getMaxTxPoWSize() {
		return mCurrentMaxTxPoWSize;
	}
	
	public MiniNumber getMaxKISSOps() {
		return mCurrentMaxKISSVMOps;
	}
	
	public MiniNumber getMaxNumTxns() {
		return mCurrentMaxTxnPerBlock;
	}
	
	public MiniData getMinTxPowWork() {
		return mCurrentMinTxPoWWork;
	}
	
	/**
	 * Calculate the current MAX values by taking a heavily weighted average
	 * 
	 *  Desired MUST be >= x0.5 and <= x2
	 *  
	 */
	public Magic calculateNewCurrent() {
		
		//The New Magic Numbers
		Magic ret = new Magic();
		
		//TxPoWSize
		MiniNumber desired 	= mDesiredMaxTxPoWSize;
		MiniNumber min	 	= mCurrentMaxTxPoWSize.div(MiniNumber.TWO);
		MiniNumber max	 	= mCurrentMaxTxPoWSize.mult(MiniNumber.TWO);
		if(desired.isLess(min)) {
			desired = min;
		}else if(desired.isMore(max)) {
			desired = max;
		
		}
		
		//And finally - this is the minimum limit
		if(desired.isLess(MINMAX_TXPOW_SIZE)) {
			desired = MINMAX_TXPOW_SIZE;
		}
		
		ret.mCurrentMaxTxPoWSize 	= mCurrentMaxTxPoWSize.mult(CALC_WEIGHTED).add(desired).div(CALC_TOTAL);
		
		//KISSVMOpS
		desired 	= mDesiredMaxKISSVMOps;
		min	 		= mCurrentMaxKISSVMOps.div(MiniNumber.TWO);
		max	 		= mCurrentMaxKISSVMOps.mult(MiniNumber.TWO);
		if(desired.isLess(min)) {
			desired = min;
		}else if(desired.isMore(max)) {
			desired = max;
		}
		
		//And finally - this is the minimum limit
		if(desired.isLess(MINMAX_KISSVM_OPERATIONS)) {
			desired = MINMAX_KISSVM_OPERATIONS;
		}
		
		ret.mCurrentMaxKISSVMOps	= mCurrentMaxKISSVMOps.mult(CALC_WEIGHTED).add(desired).div(CALC_TOTAL);
		
		//Txns per block
		desired 	= mDesiredMaxTxnPerBlock;
		min	 		= mCurrentMaxTxnPerBlock.div(MiniNumber.TWO);
		max	 		= mCurrentMaxTxnPerBlock.mult(MiniNumber.TWO);
		if(desired.isLess(min)) {
			desired = min;
		}else if(desired.isMore(max)) {
			desired = max;
		}
		
		//And finally - this is the minimum limit
		if(desired.isLess(MINMAX_TXPOW_TXNS)) {
			desired = MINMAX_TXPOW_TXNS;
		}
		
		ret.mCurrentMaxTxnPerBlock	= mCurrentMaxTxnPerBlock.mult(CALC_WEIGHTED).add(desired).div(CALC_TOTAL);
		
		//Work is slightly different as is MiniData
		BigInteger two 	  = new BigInteger("2");
		BigInteger oldval = mCurrentMinTxPoWWork.getDataValue();
		BigInteger minval = oldval.divide(two);
		BigInteger maxval = oldval.multiply(two);
		
		BigInteger newval = mDesiredMinTxPoWWork.getDataValue();
		if(newval.compareTo(minval)<0) {
			newval = minval;
		}else if(newval.compareTo(maxval)>0) {
			newval = maxval;
		}
		
		//And finally - this is the minimum limit
		if(newval.compareTo(MIN_TXPOW_VAL) > 0) {
			newval = MIN_TXPOW_VAL;
		}
		
		//Now do the same calculation..
		BigInteger calc = oldval.multiply(CALC_WEIGHTED.getAsBigInteger()).add(newval).divide(CALC_TOTAL.getAsBigInteger()); 
		ret.mCurrentMinTxPoWWork = new MiniData(calc);	
		
		return ret;
	}
	
	@Override
	public String toString() {
		return toJSON().toString();
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mCurrentMaxTxPoWSize.writeDataStream(zOut);
		mCurrentMaxKISSVMOps.writeDataStream(zOut);
		mCurrentMaxTxnPerBlock.writeDataStream(zOut);
		mCurrentMinTxPoWWork.writeDataStream(zOut);
		
		mDesiredMaxTxPoWSize.writeDataStream(zOut);
		mDesiredMaxKISSVMOps.writeDataStream(zOut);
		mDesiredMaxTxnPerBlock.writeDataStream(zOut);
		mDesiredMinTxPoWWork.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mCurrentMaxTxPoWSize	= MiniNumber.ReadFromStream(zIn);
		mCurrentMaxKISSVMOps	= MiniNumber.ReadFromStream(zIn);
		mCurrentMaxTxnPerBlock 	= MiniNumber.ReadFromStream(zIn);
		mCurrentMinTxPoWWork 	= MiniData.ReadFromStream(zIn);
		
		mDesiredMaxTxPoWSize	= MiniNumber.ReadFromStream(zIn);
		mDesiredMaxKISSVMOps	= MiniNumber.ReadFromStream(zIn);
		mDesiredMaxTxnPerBlock 	= MiniNumber.ReadFromStream(zIn);
		mDesiredMinTxPoWWork	= MiniData.ReadFromStream(zIn);
	}
	
	public static Magic ReadFromStream(DataInputStream zIn) throws IOException {
		Magic mag = new Magic();
		mag.readDataStream(zIn);
		return mag;
	}
	
	public static void main(String[] zArgs) {

		MiniNumber desired 	= new MiniNumber(3000);
	
		System.out.println("Start:1024 Desired:"+desired);
		
		int days=0;
		Magic current 		= new Magic();
//		current.mCurrentMaxKISSVMOps = new MiniNumber(2000);
		for(int i=0;i<1728*50;i++) {
			if(i%1000==0) {
				current.mDesiredMaxKISSVMOps = desired;
			}
			
			current = current.calculateNewCurrent();
			
			if(i%50 == 0) {
				days++;
				System.out.println(days+") "+current.mCurrentMaxKISSVMOps);
			}
		}	
	}
}
