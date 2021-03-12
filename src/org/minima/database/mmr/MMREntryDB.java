package org.minima.database.mmr;

import java.util.Enumeration;
import java.util.Hashtable;

import org.minima.objects.base.MiniNumber;

/**
 * A single copy of each MMR Entry so that duplicates do not take up space
 * 
 * @author spartacusrex
 */
public class MMREntryDB {

	/**
	 * Create a static version for all MMRSets to access..
	 */
	private static MMREntryDB mDB = new MMREntryDB();
	public static MMREntryDB getDB() {
		return mDB;
	}
	
	/**
	 * All the shared entries across al the MMRSets
	 */
	private Hashtable<String, MMREntryDBRow> mAllEntries;
	
	private class MMREntryDBRow {
		MMREntry mEntry;
		MiniNumber mMaxBlock;
		
		public MMREntryDBRow(MMREntry zEntry, MiniNumber zMaxBlock) {
			mEntry    = zEntry;
			mMaxBlock = zMaxBlock;
		}
		
		public MMREntry getEntry() {
			return mEntry;
		}
		
		public MiniNumber getMaxBloxk(){
			return mMaxBlock;
		}
		
		public void checkMaxBlock(MiniNumber zMax) {
			if(zMax.isMore(mMaxBlock)) {
				mMaxBlock = zMax;
			}
		}
	}
	
	/**
	 * Private constructor as can only access via the static getDB()
	 */
	private MMREntryDB() {
		mAllEntries = new Hashtable<>();	
	}
	
	/**
	 * Get the Location of an ENtry
	 * 
	 * @param zEntry
	 * @return the String Location
	 */
	private String getTableEntry(MMREntry zEntry) {
		boolean dataonly     = zEntry.getData().isHashOnly();
		String 		 hash    = zEntry.getHashValue().to0xString();
//		MiniNumber  valuesum = zEntry.getData().getValueSum();
		
		//Convert this MMREntry into a unique string..
//		return hash+":"+dataonly+":"+valuesum;
		return hash+":"+dataonly;
	}
	
	/**
	 * If we already have this entry return it..
	 * Else add it and return it..
	 * 
	 * @param zEntry
	 * @return
	 */
	public MMREntry getEntry(MMREntry zEntry, MiniNumber zBlock) {
		//Convert this MMREntry into a unique string..
		String loc = getTableEntry(zEntry);
		
		//Do we have it..
		MMREntryDBRow oldentry = mAllEntries.get(loc);
		
		//Valid.. ?
		if(oldentry != null) {
			oldentry.checkMaxBlock(zBlock);
			return oldentry.getEntry();
		}
		
		//Create a new Entry
		MMREntryDBRow row = new MMREntryDBRow(zEntry, zBlock);
		
		//Else add it.. for next time..
		mAllEntries.put(loc, row);
		
		//And return it..
		return zEntry;
	}
	
	/**
	 * Wipe the DB
	 */
	public void clearDB() {
		mAllEntries.clear();
	}
	
	public int getSize() {
		return mAllEntries.size();
	}
	
	/**
	 * Remove entries that are no  longer used.. past the cascade node
	 * @param mMinNumber
	 */
	public void cleanUpDB(MiniNumber zMinNumber) {
		//The NEW Table..
		Hashtable<String, MMREntryDBRow> allEntries = new Hashtable<>();
		
		//Run through and keep the good ones..
		Enumeration<MMREntryDBRow> entries = mAllEntries.elements();
		while(entries.hasMoreElements()) {
			MMREntryDBRow row = entries.nextElement();
			MiniNumber block  = row.getMaxBloxk();
			
			if(block.isMoreEqual(zMinNumber)) {
				String loc = getTableEntry(row.getEntry());
				allEntries.put(loc, row);
			}
		}
		
		//And switch the 2..
		mAllEntries = allEntries;
	}
}
