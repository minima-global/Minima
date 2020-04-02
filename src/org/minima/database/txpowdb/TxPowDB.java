package org.minima.database.txpowdb;

import java.util.ArrayList;

import org.minima.objects.TxPOW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

public interface TxPowDB {
	
	/**
	 * Add a TxPOW to the database if it is not already there
	 * 
	 * Returns the row
	 * 
	 * @param zTxPOW
	 */
	public TxPOWDBRow addTxPOWDBRow(TxPOW zTxPOW);
	
	/**
	 * Search for a specific TxPOW
	 * 
	 * @param zTxID
	 * @return the TxPOW if found or NULL
	 */
	public TxPOWDBRow findTxPOWDBRow(MiniData zTxPOWID);
	
	public ArrayList<TxPOWDBRow> getAllTxPOWDBRow();
	
	public void resetAllInBlocks();
	
	
	/**
	 * Remove a single row
	 * @param zTxPOWID
	 */
	public void removeTxPOW(MiniData zTxPOWID);
	
	/**
	 * Remove all TxPOW rows from the database that are in blocks earlier than this.
	 * Once you cascade you can't go back, so those TXPOW will never change.
	 * 
	 * @param zTxPOWID
	 */
	public ArrayList<TxPOWDBRow> removeTxPOWInBlockLessThan(MiniNumber zBlockNumber);
	
	public ArrayList<TxPOWDBRow> getAllUnusedTxPOW();
	
	public ArrayList<TxPOWDBRow> getChildBlocksTxPOW(MiniData zParent);
	
	public ArrayList<TxPOWDBRow> getAllBlocksMissingTransactions();
	
	public int getCompleteSize();
	
	/**
	 * Clear the DB
	 */
	public void ClearDB();
}
