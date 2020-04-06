package org.minima.database.mmr;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniInteger;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;

public class MMREntry implements Comparable<MMREntry>, Streamable{

	/**
	 * Global MMR position
	 */
	
	MiniInteger mEntryNumber;
	int mRow;
	
	/**
	 * The blocktime..
	 */
	MiniNumber mBlockTime = new MiniNumber(0);
	
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
	public MMREntry(int zRow, MiniInteger zEntry) {
		mRow = zRow;
		mEntryNumber = zEntry;
		mIsEmpty = true;
	}
	
	public boolean isEmpty() {
		return mIsEmpty;
	}
	
	public boolean checkPosition(int zRow, MiniInteger zEntry) {
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
	
	public MiniData getHashValue() {
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
	public MiniInteger getEntry() {
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
		return mEntryNumber.modulo(MiniInteger.TWO).isEqual(MiniInteger.ZERO);
	}
	
	public boolean isRight() {
		return !isLeft();
	}
	
	public MiniInteger getLeftSibling() {
		return mEntryNumber.sub(MiniInteger.ONE);
	}
	
	public MiniInteger getRightSibling() {
		return mEntryNumber.add(MiniInteger.ONE);
	}
	
	public MiniInteger getSibling() {
		if(isLeft()) {
			return getRightSibling();
		}else {
			return getLeftSibling();
		}
	}
	
	public MiniInteger getParentEntry() {
		return mEntryNumber.divRoundDown(MiniInteger.TWO);
	}
	
	public MiniInteger getLeftChildEntry() {
		return mEntryNumber.mult(MiniInteger.TWO);
	}
	
	public MiniInteger getRightChildEntry() {
		return getLeftChildEntry().add(MiniInteger.ONE);
	}

	@Override
	public int compareTo(MMREntry zEntry) {
		return zEntry.getEntry().getNumber().compareTo(mEntryNumber.getNumber());
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
		mEntryNumber = MiniInteger.ReadFromStream(zIn);
		mRow         = zIn.readInt();
		mData        = MMRData.ReadFromStream(zIn);
		mIsEmpty     = false;
	}
}
