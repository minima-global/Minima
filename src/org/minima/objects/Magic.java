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
	 * Maximum size of a TxPoW unit..
	 */
	private static final MiniNumber MAX_TXPOW_SIZE 		= new MiniNumber(64*1024);
	
	/**
	 * Maximum Number of executed KISSVM Operations
	 */
	private static final MiniNumber MAX_KISSVM_OPERATIONS = new MiniNumber(1024);
	
	/**
	 * Maximum number of Txns per block
	 */
	private static final MiniNumber MAX_TXPOW_TXNS		 = new MiniNumber(100);
	
	/**
	 * Minimum acceptable PoW per TxPoW
	 */
	private static final BigInteger MEGA_VAL 			 = Crypto.MAX_VAL.divide(new BigInteger("1000"));	
	private static final MiniData   MIN_TXPOW_WORK		 = new MiniData("0x"+MEGA_VAL.toString(16));
		
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
		mCurrentMaxTxPoWSize			= MAX_TXPOW_SIZE;
		mCurrentMaxKISSVMOps			= MAX_KISSVM_OPERATIONS;
		mCurrentMaxTxnPerBlock			= MAX_TXPOW_TXNS;
		mCurrentMinTxPoWWork			= MIN_TXPOW_WORK;
		
		mDesiredMaxTxPoWSize			= MAX_TXPOW_SIZE;
		mDesiredMaxKISSVMOps			= MAX_KISSVM_OPERATIONS;
		mDesiredMaxTxnPerBlock        	= MAX_TXPOW_TXNS;
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
	
	/**
	 * The user votes on what he thinks it should be..
	 * 
	 * MUST BE >= x0.5 and <= x2 of the current values.
	 */
	public boolean checkValid() {
		
		MiniNumber ratio = mDesiredMaxTxPoWSize.div(mCurrentMaxTxPoWSize);
		if(ratio.isMore(MiniNumber.TWO) || ratio.isLess(MiniNumber.HALF)) {
			return false;
		} 
		
		ratio = mDesiredMaxTxnPerBlock.div(mCurrentMaxTxnPerBlock);
		if(ratio.isMore(MiniNumber.TWO) || ratio.isLess(MiniNumber.HALF)) {
			return false;
		}
		
		ratio = mDesiredMaxKISSVMOps.div(mCurrentMaxKISSVMOps);
		if(ratio.isMore(MiniNumber.TWO) || ratio.isLess(MiniNumber.HALF)) {
			return false;
		}
		
		//Check Min TxPoW..
		BigInteger two 			= new BigInteger("2");
		BigInteger currentval 	= mCurrentMinTxPoWWork.getDataValue();
		BigInteger desiredval 	= mDesiredMinTxPoWWork.getDataValue();
		
		//If more than * 2 or less than * 0.5.. return false
		if(desiredval.compareTo(currentval.multiply(two))>0) {
			return false;
		}else if(desiredval.compareTo(currentval.divide(two))<0) {
			return false;
		}
		
		return true;
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
	 */
	public void calculateNewCurrent(Magic zParentMagic) {
		
		MiniNumber parent 		= zParentMagic.getMaxTxPoWSize();
		mCurrentMaxTxPoWSize	= parent.mult(CALC_WEIGHTED).add(mDesiredMaxTxPoWSize).div(CALC_TOTAL);
		
		parent 					= zParentMagic.getMaxKISSOps();
		mCurrentMaxKISSVMOps	= parent.mult(CALC_WEIGHTED).add(mDesiredMaxKISSVMOps).div(CALC_TOTAL);
		
		parent 					= zParentMagic.getMaxNumTxns();
		mCurrentMaxTxnPerBlock 	= parent.mult(CALC_WEIGHTED).add(mDesiredMaxTxnPerBlock).div(CALC_TOTAL);
	
		//Work is slightly different as is MiniData
		BigInteger oldval = zParentMagic.getMinTxPowWork().getDataValue();
		BigInteger newval = mDesiredMinTxPoWWork.getDataValue();
		
		//Now do the same calculation..
		BigInteger calc = oldval.multiply(CALC_WEIGHTED.getAsBigInteger()).add(newval).divide(CALC_TOTAL.getAsBigInteger()); 
		mCurrentMinTxPoWWork = new MiniData(calc);	
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
		
		Magic oldparam = new Magic();
		
		Magic newparam = new Magic();
		newparam.mDesiredMaxTxnPerBlock = new MiniNumber(200);
		newparam.mDesiredMinTxPoWWork = new MiniData("0x3189374BC6A7EF9DB22D0E5604189374BC6A7EF9DB22D0E5604189374BC6A7");
		
		newparam.calculateNewCurrent(oldparam);
		System.out.println(newparam.toJSON());
		
		for(int i=0;i<100;i++) {
			
			newparam.calculateNewCurrent(newparam);
			
			System.out.println(i+") "+newparam.toJSON());
		}
		
	}
}
