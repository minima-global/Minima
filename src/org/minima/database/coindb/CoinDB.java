package org.minima.database.coindb;

import java.util.ArrayList;

import org.minima.objects.Coin;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

public interface CoinDB {

	/**
	 * Clear the database
	 */
	public void clearDB();
//	public void clearOldCoins(long zCurrentBlock);
	
	/**
	 * SELECT *
	 * @return
	 */
	ArrayList<CoinDBRow> getComplete();
	
	/**
	 * Return all coins that have this address
	 * 
	 * @param zAddress
	 * @return all coins that have this address
	 */
	public ArrayList<CoinDBRow> checkForRelevantCoins(MiniData zAddress);
	
	/**
	 * Get the coin row with this CoinID or NULL if none present
	 * 
	 * @param zCoin
	 * @return
	 */
	public CoinDBRow getCoinRow(MiniData zCoinID);
	
	/**
	 * Add a coinRow
	 * 
	 * @param zCoin
	 * @return
	 */
	public CoinDBRow addCoinRow(Coin zCoin);
	
	public void removeOldSpentCoins(MiniNumber zMinBlock);
}
