package org.minima.database.txpowdb;

import org.minima.objects.TxPOW;
import org.minima.objects.base.MiniNumber;

public interface TxPOWDBRow {
	
	/**
	 * The states a TXPOW block entry can be in
	 */
	
	/**
	 * An uchecked block is added
	 */
	public static int TXPOWDBROW_STATE_BASIC   = 0;
	
	/**
	 * This block has ALL it's transctions available
	 */
	public static int TXPOWDBROW_STATE_FULL    = 1;
	
//	/**
//	 * This is a valid block.. all transaction check. It's parent is valid.
//	 */
//	public static int TXPOWDBROW_STATE_VALID   = 2;
//	
//	/**
//	 * This is a IN-valid block.. all child blocks are INVALID.
//	 */
//	public static int TXPOWDBROW_STATE_INVALID = 3;
//	
	/**
	 * The TXPOW
	 * @return
	 */
	public TxPOW getTxPOW();
	
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
	public boolean isOnChainBlock();
	public void setOnChainBlock(boolean zOnChainBlock);
	
	/**
	 * What state is this in
	 */
	public int getBlockState();
	public void setBlockState(int zState);
}
