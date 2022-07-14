package org.minima.database.mmr;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.objects.base.MiniNumber;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;

public class MMREntry implements Streamable {

	/**
	 * Global MMR position
	 */
	MMREntryNumber 	mEntryNumber;
	int 		mRow;
	
	/**
	 * The data stored here
	 */
	MMRData    mMMRData;
	
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
	private MMREntry() {}
	
	public MMREntry(int zRow, MMREntryNumber zEntry) {
		mRow 			= zRow;
		mEntryNumber 	= zEntry;
		mIsEmpty 		= true;
	}
	
	public MMREntry(int zRow, MMREntryNumber zEntry, MMRData zMMRData) {
		mRow 			= zRow;
		mEntryNumber 	= zEntry;
		mMMRData 		= zMMRData;
		mIsEmpty 		= false;
	}
	
	public MMREntryNumber getEntryNumber() {
		return mEntryNumber;
	}
	
	public int getRow() {
		return mRow;
	}
	
	public MMRData getMMRData() {
		return mMMRData;
	}
	
	public boolean isEmpty() {
		return mIsEmpty;
	}
	
	public boolean checkPosition(MMREntry zEntry) {
		return (zEntry.getRow() == mRow) && zEntry.getEntryNumber().isEqual(mEntryNumber);
	}
	
	public boolean checkPosition(ArrayList<MMREntry> zMultipleEntries) {
		for(MMREntry entry : zMultipleEntries) {
			if(checkPosition(entry)) {
				return true;
			}
		}
		return false;
	}
	
	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		
		ret.put("row", mRow);
		ret.put("entry", mEntryNumber.toString());
		ret.put("data", mMMRData.toJSON());
		
		return ret;
	}
	
	@Override
	public String toString() {
		return toJSON().toString();
	}
	
	/**
	 * 
	 * UTILITY FUNCTIONS FOR NAVIGATING THE MMR
	 * 
	 */
	public int getParentRow() {
		return mRow+1;
	}
	
	public int getChildRow() {
		return mRow-1;
	}
	
	public boolean isLeft() {
		return mEntryNumber.modulo(MMREntryNumber.TWO).isEqual(MMREntryNumber.ZERO);
	}
	
	public boolean isRight() {
		return !isLeft();
	}
	
	public MMREntryNumber getLeftSibling() {
		return mEntryNumber.decrement();
	}
	
	public MMREntryNumber getRightSibling() {
		return mEntryNumber.increment();
	}
	
	public MMREntryNumber getSibling() {
		if(isLeft()) {
			return mEntryNumber.increment();
		}else {
			return mEntryNumber.decrement();
		}
	}
	
	public MMREntryNumber getParentEntry() {
		return mEntryNumber.div2().floor();
	}
	
	public MMREntryNumber getLeftChildEntry() {
		return mEntryNumber.mult2();
	}
	
	public MMREntryNumber getRightChildEntry() {
		return getLeftChildEntry().increment();
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		//The Row..
		MiniNumber row = new MiniNumber(mRow);
		row.writeDataStream(zOut);
		
		//Entry number
		mEntryNumber.writeDataStream(zOut);
		
		//And finally the data
		mMMRData.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mIsEmpty 	 = false;
		mRow         = MiniNumber.ReadFromStream(zIn).getAsInt();
		mEntryNumber = MMREntryNumber.ReadFromStream(zIn);
		mMMRData     = MMRData.ReadFromStream(zIn);
	}
	
	public static MMREntry ReadFromStream(DataInputStream zIn) throws IOException{
		MMREntry entry = new MMREntry();
		entry.readDataStream(zIn);
		return entry;	
	}
	
}
