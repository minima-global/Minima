package org.minima;

import org.minima.objects.base.MiniNumber;

public class GlobalParams {

	/**
	 * Which Version
	 */
	public static final String MINIMA_VERSION = "0.96.51"; 
	
	/**
	 * Number of seconds before sending a pulse message - every 10 minutes
	 */
	public static final int USER_PULSE_FREQ   = 10 * 60;
	
	/**
	 * Speed in blocks per second.. 
	 * -  0.05  = 20 second block time
	 */
	public static final MiniNumber MINIMA_BLOCK_SPEED  = new MiniNumber("0.05");
	
	/**
	 * When checking speed and average difficulty only look at this many blocks back
	 */
	public static final MiniNumber MINIMA_BLOCKS_SPEED_CALC = new MiniNumber(1024);
	
	/**
	 * How deep before we think confirmed..
	 */
	public static final MiniNumber MINIMA_CONFIRM_DEPTH  = new MiniNumber("3");
	
	/**
	 * Depth before we cascade..
	 */
	public static final MiniNumber MINIMA_CASCADE_START_DEPTH = new MiniNumber(16384);
	
	/**
	 * Number of blocks at each cascade level 
	 */
	public static final int MINIMA_CASCADE_LEVEL_NODES  = 256;
	
	/**
	 * How Many Cascade Levels
	 */
	public static final int MINIMA_CASCADE_LEVELS  = 21;
	
	/**
	 * Current default HASH_Strength Used. Can be up to 512.
	 * All the MINING, TxPoW and MMR data ALWAYS uses 512. But addresses, scripts, and public keys..
	 * can be set to less. This way signatures and addresses are shorter.
	 */
	public static final int MINIMA_DEFAULT_HASH_STRENGTH = 160;
	
	/**
	 * Max Proof History - how far back to use a proof of coin..
	 * If there is a re-org of more than this the proof will be invalid 
	 */
	public static final MiniNumber MINIMA_MMR_PROOF_HISTORY = new MiniNumber(256);
	
	/**
	 * Are we debugging the chain in short chain mode..
	 */
	public static final boolean SHORT_CHAIN_DEBUG_MODE = false;
	
	/**
	 * Just create a block every transaction. Useful when not mining 
	 * and just want a block every single transaction to debug.
	 * Automatically disables the auto mining
	 */
	public static final boolean MINIMA_ZERO_DIFF_BLK  = false;

//	/** 
//	 * ( Not sure if I'll use this..)
//	 * MAX Difficulty change per block
//	 */
//	public static final MiniNumber MINIMA_MAX_SPEED_RATIO  = new MiniNumber("0.5");
	

}
