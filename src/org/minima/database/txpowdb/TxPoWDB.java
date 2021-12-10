package org.minima.database.txpowdb;

import java.io.File;
import java.util.ArrayList;

import org.minima.database.txpowdb.ram.RamDB;
import org.minima.database.txpowdb.sql.TxPoWSqlDB;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;

/**
 * The Main TxPoW store for the whole app
 * 
 * TxPoW are added and kept for a certain period. 
 * 
 * That period is up to the Node runner. The node won't need the txpow 
 * after a day but may choose to keep them all for longer periods.
 * 
 * Helpful for resyncing clients as they come to the network.
 * 
 * Store long term in SQL but kept in RAM for short term fast access..
 * 
 */
public class TxPoWDB {

	RamDB mRamDB;
	TxPoWSqlDB mSqlDB;
	
	public TxPoWDB() {
		mRamDB = new RamDB();
		mSqlDB = new TxPoWSqlDB();
	}
	
	public void loadSQLDB(File zFile) {
		//Set the SQL DB base file
		mSqlDB.loadDB(zFile);
	}
	
	public void saveDB() {
		//Shut down the SQL DB cleanly
		mSqlDB.saveDB();
	}
	
	/**
	 * Add a TxPoW to the Database - both RAM and SQL
	 */
	public void addTxPoW(TxPoW zTxPoW) {
		//Get the ID
		String txpid = zTxPoW.getTxPoWID();
		
		//Do we have it already..
		if(!mRamDB.exists(txpid)) {
			//Is it in the SQL
			if(!mSqlDB.exists(txpid)) {
				//Add it to the SQL..
				mSqlDB.addTxPoW(zTxPoW);
			}
			
			//Add it to the RAM
			mRamDB.addTxPoW(zTxPoW);
		}
	}
	
	public void addSQLTxPoW(TxPoW zTxPoW) {
		//Get the ID
		String txpid = zTxPoW.getTxPoWID();
		
		//Is it in the SQL
		if(!mSqlDB.exists(txpid)) {
			//Add it to the SQL..
			mSqlDB.addTxPoW(zTxPoW);
		}
	}
	
	/**
	 * Find a specific TxPoW
	 */
	public TxPoW getTxPoW(String zTxPoWID) {
		//First check the fast RAM DB
		TxPoW txp = mRamDB.getTxPoW(zTxPoWID);
		
		//An Old TxPoW?
		if(txp == null) {
			//Check the SQL..
			txp = mSqlDB.getTxPoW(zTxPoWID);
			
			//If found add to RamDB
			if(txp != null) {
				//For fast access next time
				mRamDB.addTxPoW(txp);
			}
		}
		
		//Could still be null
		return txp;
	}
	
	public ArrayList<TxPoW> getAllTxPoW(ArrayList<String> zTxPoWID) {
		ArrayList<TxPoW> ret = new ArrayList<>();
		
		//Cycle through the list
		for(String child : zTxPoWID) {
			
			//Could be in RAM already
			TxPoW txp = getTxPoW(child);
			
			//only add if valid..
			if(txp != null) {
				ret.add(txp);
			}
		}
		
		return ret;
	}

	/**
	 * Do we have this TxPoW - no need to load it. just check if we have it
	 * 
	 * @param zTxPoWID
	 * @return
	 */
	public boolean exists(String zTxPoWID) {
		//Is it in RAM
		boolean exists = mRamDB.exists(zTxPoWID);
		
		//If not check the SQL
		if(!exists) {
			exists = mSqlDB.exists(zTxPoWID);
		}
		
		return exists;
	}
	
	/**
	 * Find the children of a TxPoW ( ONLY TxPoW BLocks)
	 */
	public ArrayList<TxPoW> getChildBlocks(String zParentTxPoWID){
		//Ask the SQL for all the children first..
		ArrayList<String> children = mSqlDB.getChildBlocks(zParentTxPoWID);
		
		//Now get all of those..
		return getAllTxPoW(children);
	}
	
	/**
	 * How big is the DB
	 */
	public int getRamSize() {
		return mRamDB.getSize();
	}
	
	public int getSqlSize() {
		return mSqlDB.getSize();
	}
	
	public File getSqlFile() {
		return mSqlDB.getSQLFile();
	}
	
	public TxPoWSqlDB getSQLDB() {
		return mSqlDB;
	}
	
	/**
	 * Remove OLD TxPoWs from the DB - no longer needed..
	 * 
	 * When you access a txpow it's record is updated 
	 * and will not be deleted for another time period 
	 */
	public void cleanDB() {
		mRamDB.cleanDB();
		mSqlDB.cleanDB();
	}
	
	/**
	 * MEMPOOL specific functions
	 */
	public void clearMainChainTxns() {
		mRamDB.clearMainChainTxns();
	}
	
	public void setOnMainChain(String zTxPoWID) {
		mRamDB.setOnMainChain(zTxPoWID);
	}
	
	public void setInCascade(String zTxPoWID) {
		mRamDB.setInCascade(zTxPoWID);
	}
	
	public ArrayList<TxPoW> getAllUnusedTxns(){
		return mRamDB.getAllUnusedTxns();
	}
	
	/**
	 * Remove a TxPoW from the RamDB (Mempool)
	 */
	public void removeMemPoolTxPoW(String zTxPoWID) {
		mRamDB.remove(zTxPoWID);
	}
	
	/**
	 * Check for a certain CoinID - double spend
	 */
	public boolean checkMempoolCoins(MiniData zCoinID) {
		return mRamDB.checkForCoinID(zCoinID);
	}
}
