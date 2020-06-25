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
	
	/**
	 * SELECT *
	 * @return
	 */
	ArrayList<CoinDBRow> getComplete();
	
	/**
	 * SELECT *
	 * @return
	 */
	ArrayList<CoinDBRow> getCompleteRelevant();
	
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
	
	/**
	 * Can remove a Coin Row..
	 */
	public boolean removeCoin(MiniData zCoinID);
	
	/**
	 * remove old spent coins that are before this block..
	 * @param zMinBlock
	 */
	public void removeOldSpentCoins(MiniNumber zMinBlock);
	
	/**
	 * remove coins added from this b lock onwards
	 * @param zBlock
	 */
	public void resetCoinsFomOnwards(MiniNumber zBlock);
	
}
