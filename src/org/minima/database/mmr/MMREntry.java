package org.minima.database.mmr;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

import org.minima.objects.base.MiniData32;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;

public class MMREntry implements Comparable<MMREntry>, Streamable{

	/**
	 * Used many times..
	 */
	private static final BigInteger MMR_TWO 		= new BigInteger("2");
	private static final BigDecimal MMR_TWO_DEC 	= new BigDecimal("2");
	
	/**
	 * Global MMR position
	 */
	
	MiniNumber mEntryNumber;
	int mRow;
	
	/**
	 * The blocktime..
	 */
	MiniNumber mBlockTime = MiniNumber.ZERO;
	
	/**
	 * The data stored here
	 */
	MMRData    mData;
	
	/**
	 * Valid entry
	 */
	boolean mIsEmpty;
	
	/**
	 * Default constructor
	 * 
	 * @param zRow
	 * @param zEntry
	 */
	public MMREntry(int zRow, MiniNumber zEntry) {
		mRow = zRow;
		mEntryNumber = zEntry;
		mIsEmpty = true;
	}
	
	public boolean isEmpty() {
		return mIsEmpty;
	}
	
	public boolean checkPosition(int zRow, MiniNumber zEntry) {
		return (zRow == mRow) && zEntry.isEqual(mEntryNumber);
	}
	
	public boolean checkPosition(MMREntry zEntry) {
		return (zEntry.getRow() == mRow) && zEntry.getEntry().isEqual(mEntryNumber);
	}
	
	public void setData(MMRData zData) {
		mData    = zData;
		mIsEmpty = false;
	}
	
	public void clearData() {
		mIsEmpty = true;
		mData    = null;
	}
	
	public MMRData getData() {
		return mData;
	}
	
	public void setBlockTime(MiniNumber zBlockTime) {
		mBlockTime = zBlockTime;
	}
	
	public MiniNumber getBlockTime() {
		return mBlockTime;
	}
	
	public MiniData32 getHashValue() {
		if(isEmpty()) {
			MinimaLogger.log("ERROR NULL Entry : "+this);
		}
		return mData.getFinalHash();
	}
	
	@Override
	public String toString() {
		return "BLKTIME:"+mBlockTime+" R:"+mRow+" E:"+mEntryNumber+" D:"+mData;
	}
	
	/**
	 * 
	 * UTILITY FUNCTIONS FOR NAVIGATING THE MMR
	 * 
	 */
	public MiniNumber getEntry() {
		return mEntryNumber;
	}
	
	public int getRow() {
		return mRow;
	}
	
	public int getParentRow() {
		return mRow+1;
	}
	
	public int getChildRow() {
		return mRow-1;
	}
	
	public boolean isLeft() {
//		return mEntryNumber.getAsBigInteger().mod(MMR_TWO) == BigInteger.ZERO;
		return mEntryNumber.modulo(MiniNumber.TWO).isEqual(MiniNumber.ZERO);
	}
	
	public boolean isRight() {
		return !isLeft();
	}
	
	public MiniNumber getLeftSibling() {
		return mEntryNumber.sub(MiniNumber.ONE);
	}
	
	public MiniNumber getRightSibling() {
		return mEntryNumber.add(MiniNumber.ONE);
	}
	
	public MiniNumber getSibling() {
		if(isLeft()) {
			return getRightSibling();
		}else {
			return getLeftSibling();
		}
	}
	
	public MiniNumber getParentEntry() {
		//Rounds Down..
//		BigDecimal par  = mEntryNumber.getAsBigDecimal().divide(MMR_TWO_DEC,RoundingMode.DOWN);
//		return new MiniNumber(par);
		return mEntryNumber.divRoundDown(MiniNumber.TWO);
	}
	
	public MiniNumber getLeftChildEntry() {
//		BigInteger par = mEntryNumber.getAsBigInteger().multiply(MMR_TWO);
//		return new MiniNumber(par);
		return mEntryNumber.mult(MiniNumber.TWO);
	}
	
	public MiniNumber getRightChildEntry() {
		return getLeftChildEntry().add(MiniNumber.ONE);
	}

	@Override
	public int compareTo(MMREntry zEntry) {
		return zEntry.getEntry().compareTo(mEntryNumber);
//		return mEntryNumber.compareTo(zEntry.getEntry());
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		//Entry number
		mEntryNumber.writeDataStream(zOut);
		
		//The Row..
		zOut.writeInt(mRow);
		
		//And finally the data
		mData.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mEntryNumber = MiniNumber.ReadFromStream(zIn);
		mRow         = zIn.readInt();
		mData        = MMRData.ReadFromStream(zIn);
		mIsEmpty     = false;
	}
}
