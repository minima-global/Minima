package org.minima.database.txpowdb;

import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.json.JSONObject;

public interface TxPOWDBRow {
	
	/**
	 * An unchecked block is added
	 */
	public static int TXPOWDBROW_STATE_BASIC   = 0;
	
	/**
	 * This block has ALL it's transactions available
	 */
	public static int TXPOWDBROW_STATE_FULL    = 1;
	
	/**
	 * The TXPOW
	 * @return
	 */
	public TxPoW getTxPOW();
	
	/**
	 * Is this TXPOW in a block on the current longest chain
	 */
	public boolean isInBlock();
	public void setIsInBlock(boolean zIsInBlock);
	
	/**
	 * What Block is it in. Returns -1 if not in a block.
	 * @return
	 */
	public MiniNumber getInBlockNumber();
	public void setInBlockNumber(MiniNumber zBlockNumber);

	/**
	 * Is this a block on the main 
	 */
	public boolean isMainChainBlock();
	public void setMainChainBlock(boolean zOnChainBlock);
	
	/**
	 * What state is this in
	 */
	public int getBlockState();
	public void setBlockState(int zState);
	
	public JSONObject toJSON();
	
	
	/**
	 * Is this TxPoW Monotonic - do we need to recheck the Transaction Script given a different block..
	 */
	public boolean isMonoTonic();
	public void setMonotonic(boolean zMonotonic);
	
	/**
	 * Assume Valid
	 */
	public boolean isAssumeValid();
	public void setAssumeValid(boolean zValid);
	
	/**
	 * Has it failed a check before..
	 */
	public int getFailedAttempts();
	public void incrementFailedAttempts();
	
}
