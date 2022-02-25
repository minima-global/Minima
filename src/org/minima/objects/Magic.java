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
	public static final MiniNumber MAX_TXPOW_SIZE 		= new MiniNumber(64*1024);
	
	/**
	 * Maximum Number of executed KISSVM Operations
	 */
	public static final MiniNumber MAX_KISSVM_OPERATIONS = new MiniNumber(1024);
	
	/**
	 * Maximum number of Txns per block
	 */
	public static final MiniNumber MAX_TXPOW_TXNS		 = new MiniNumber(100);
	
	/**
	 * Minimum acceptable PoW per TxPoW
	 */
	public static final BigInteger MEGA_VAL 			 = Crypto.MAX_VAL.divide(new BigInteger("1000"));	
	public static final MiniData   MIN_TXPOW_WORK		 = new MiniData("0x"+MEGA_VAL.toString(16));
		
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
		
		//Check desired txns in block
		if(mDesiredMaxTxnPerBlock.isMore(mCurrentMaxTxnPerBlock.mult(MiniNumber.TWO))) {
			return false;
		}else if(mDesiredMaxTxnPerBlock.isLess(mCurrentMaxTxnPerBlock.div(MiniNumber.TWO))) {
			return false;
		} 
		
		//Check Min TxPoW..
		
		
		return true;
	}
	
	public boolean checkSame(Magic zMagic) {
		boolean y = mCurrentMaxTxnPerBlock.isEqual(zMagic.mCurrentMaxTxnPerBlock);
		boolean w = mCurrentMinTxPoWWork.isEqual(zMagic.mCurrentMinTxPoWWork);
		
		return y && w;
	}
	
	/**
	 * Get the Magic Parameters
	 */
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
		
		// ( 16383*old + new ) / 16384 .. very simple
		MiniNumber parent 		= zParentMagic.getMaxNumTxns();
		mCurrentMaxTxnPerBlock 	= parent.mult(CALC_WEIGHTED).add(mDesiredMaxTxnPerBlock).div(CALC_TOTAL);
	
		//Work is slightly different as is MiniData
		BigInteger oldval = zParentMagic.getMinTxPowWork().getDataValue();
		BigInteger newval = mDesiredMinTxPoWWork.getDataValue();
		
		//Now do the same calculation..
		BigInteger calc = oldval.multiply(new BigInteger("16383")).add(newval).divide(new BigInteger("16384")); 
		mCurrentMinTxPoWWork = new MiniData(calc);	
	}
	
	@Override
	public String toString() {
		return toJSON().toString();
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mCurrentMaxTxnPerBlock.writeDataStream(zOut);
		mCurrentMinTxPoWWork.writeDataStream(zOut);
		
		mDesiredMaxTxnPerBlock.writeDataStream(zOut);
		mDesiredMinTxPoWWork.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mCurrentMaxTxnPerBlock = MiniNumber.ReadFromStream(zIn);
		mCurrentMinTxPoWWork = MiniData.ReadFromStream(zIn);
		
		mDesiredMaxTxnPerBlock = MiniNumber.ReadFromStream(zIn);
		mDesiredMinTxPoWWork = MiniData.ReadFromStream(zIn);
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
		
		for(int i=0;i<16384;i++) {
			
			newparam.calculateNewCurrent(newparam);
			
			System.out.println(i+") "+newparam.toJSON());
		}
		
	}
}
