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

public class Magic implements Streamable {

	/**
	 * Used to calculate the weighted averages
	 */
	public static final MiniNumber CALC_WEIGHTED 	= new MiniNumber(16383);
	public static final MiniNumber CALC_TOTAL 		= new MiniNumber(16384);
	
	/**
	 * Default starting values..
	 */
	public static final MiniNumber MAX_TXPOW_SIZE 		= new MiniNumber(32000);
	public static final MiniNumber MAX_TXPOW_TXNS 	 	= new MiniNumber(100);
	
	/**
	 * The minimum amount of work for a TxPoW to be allowed across the network
	 */
	public static final BigInteger MEGA_VAL 			= Crypto.MAX_VAL.divide(new BigInteger("1000"));	
	public static final MiniData   MIN_TXPOW_WORK		= new MiniData("0x"+MEGA_VAL.toString(16));
	
	/**
	 * The Current MAGIC numbers.. based on a weighted average of the chain..
	 * 
	 * This is ( 9999*the last current values + 1*Desired value ) / 10000
	 * 
	 */
	public MiniNumber mCurrentMaxTxPoWSize;
	public MiniNumber mCurrentMaxTxnPerBlock;
	public MiniData   mCurrentMinTxPoWWork;
	
	/**
	 * The user votes on what he thinks it should be..
	 * 
	 * MUST BE >= x0.5 and <= x2 of the current values.
	 */
	public MiniNumber mDesiredMaxTxPoWSize;
	public MiniNumber mDesiredMaxTxnPerBlock;
	public MiniData   mDesiredMinTxPoWWork;
	
	public Magic() {
		mCurrentMaxTxPoWSize 			= MAX_TXPOW_SIZE;
		mCurrentMaxTxnPerBlock			= MAX_TXPOW_TXNS;
		mCurrentMinTxPoWWork			= MIN_TXPOW_WORK;
		
		mDesiredMaxTxPoWSize          	= MAX_TXPOW_SIZE;
		mDesiredMaxTxnPerBlock        	= MAX_TXPOW_TXNS;
		mDesiredMinTxPoWWork			= MIN_TXPOW_WORK;
	}

	public JSONObject toJSON() {
		JSONObject magic = new JSONObject();
		
		magic.put("desiredmaxtxpow", mDesiredMaxTxPoWSize.toString());
		magic.put("desiredmaxtxn", mDesiredMaxTxnPerBlock.toString());
		magic.put("desiredmintxpowwork", mDesiredMinTxPoWWork.to0xString());
		
		magic.put("maxtxpow", mCurrentMaxTxPoWSize.toString());
		magic.put("maxtxn", mCurrentMaxTxnPerBlock.toString());
		magic.put("mintxpowwork", mCurrentMinTxPoWWork.to0xString());
		
		return magic;
	}
	
	public boolean checkSame(Magic zMagic) {
		boolean x = mCurrentMaxTxPoWSize.isEqual(zMagic.mCurrentMaxTxPoWSize);
		boolean y = mCurrentMaxTxnPerBlock.isEqual(zMagic.mCurrentMaxTxnPerBlock);
		boolean w = mCurrentMinTxPoWWork.isEqual(zMagic.mCurrentMinTxPoWWork);
		
		return x && y && w;
	}
	
	/**
	 * Get the Magic Parameters
	 */
	public MiniNumber getMaxTxPoWSize() {
		return mCurrentMaxTxPoWSize;
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
		
		// ( 16383*old + new ) / 16384 .. very simple
		MiniNumber parent 		= zParentMagic.getMaxTxPoWSize();
		mCurrentMaxTxPoWSize 	= parent.mult(CALC_WEIGHTED).add(mDesiredMaxTxPoWSize).div(CALC_TOTAL);
		
		parent 					= zParentMagic.getMaxNumTxns();
		mCurrentMaxTxnPerBlock 	= parent.mult(CALC_WEIGHTED).add(mDesiredMaxTxnPerBlock).div(CALC_TOTAL);
	
		//Work is slightly differenbt as is MiniData
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
		mCurrentMaxTxPoWSize.writeDataStream(zOut);
		mCurrentMaxTxnPerBlock.writeDataStream(zOut);
		new MiniNumber(128).writeDataStream(zOut);
		mCurrentMinTxPoWWork.writeDataStream(zOut);
		
		mDesiredMaxTxPoWSize.writeDataStream(zOut);
		mDesiredMaxTxnPerBlock.writeDataStream(zOut);
		new MiniNumber(128).writeDataStream(zOut);
		mDesiredMinTxPoWWork.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mCurrentMaxTxPoWSize = MiniNumber.ReadFromStream(zIn);
		mCurrentMaxTxnPerBlock = MiniNumber.ReadFromStream(zIn);
		
		//KISS HACK
		MiniNumber.ReadFromStream(zIn);
		
		mCurrentMinTxPoWWork = MiniData.ReadFromStream(zIn);
		
		mDesiredMaxTxPoWSize = MiniNumber.ReadFromStream(zIn);
		mDesiredMaxTxnPerBlock = MiniNumber.ReadFromStream(zIn);
		
		//KISS HaCK
		MiniNumber.ReadFromStream(zIn);
		
		mDesiredMinTxPoWWork = MiniData.ReadFromStream(zIn);
	}
	
//	@Override
//	public void writeDataStream(DataOutputStream zOut) throws IOException {
//		mCurrentMaxTxPoWSize.writeDataStream(zOut);
//		mCurrentMaxTxnPerBlock.writeDataStream(zOut);
//		mCurrentMinTxPoWWork.writeDataStream(zOut);
//		
//		mDesiredMaxTxPoWSize.writeDataStream(zOut);
//		mDesiredMaxTxnPerBlock.writeDataStream(zOut);
//		mDesiredMinTxPoWWork.writeDataStream(zOut);
//	}
//
//	@Override
//	public void readDataStream(DataInputStream zIn) throws IOException {
//		mCurrentMaxTxPoWSize 	= MiniNumber.ReadFromStream(zIn);
//		mCurrentMaxTxnPerBlock 	= MiniNumber.ReadFromStream(zIn);
//		mCurrentMinTxPoWWork 	= MiniData.ReadFromStream(zIn);
//		
//		mDesiredMaxTxPoWSize 	= MiniNumber.ReadFromStream(zIn);
//		mDesiredMaxTxnPerBlock 	= MiniNumber.ReadFromStream(zIn);
//		mDesiredMinTxPoWWork 	= MiniData.ReadFromStream(zIn);
//	}
	
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
