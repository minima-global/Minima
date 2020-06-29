package org.minima;

import org.minima.objects.base.MiniNumber;

public class GlobalParams {

	/**
	 * Which Version
	 */
	public static final String MINIMA_VERSION = "0.95.16"; 
	
	/**
	 * Number of seconds before sending a pulse message
	 */
	public static final int USER_PULSE_FREQ      = 10 * 60;
	
	/**
	 * Just create a block every transaction. Useful when not mining 
	 * and just want a block every single transaction to debug.
	 * Automatically disables the auto mining
	 */
	public static final boolean MINIMA_ZERO_DIFF_BLK  = false;
	
	/**
	 * Speed in blocks per second.. 0.05 = 20 second block time
	 */
	public static final MiniNumber MINIMA_BLOCK_SPEED  = new MiniNumber("0.05");
	
	/**
	 * When checking speed and average difficulty only look at this many blocks back
	 * At 20 second blocks.. 720 is 4 hours
	 */
	public static final MiniNumber MINIMA_BLOCKS_SPEED_CALC = new MiniNumber(720);
	
	/**
	 * How deep before we think confirmed..
	 */
	public static final MiniNumber MINIMA_CONFIRM_DEPTH  = new MiniNumber("3");
	
	/**
	 * Depth before we cascade..
	 */
	public static final int MINIMA_CASCADE_START_DEPTH = 8192;
	
	/**
	 * Number of blocks at each cascade level 
	 */
	public static final int MINIMA_CASCADE_LEVEL_NODES  = 256;
	
	/**
	 * How Many Cascade Levels
	 */
	public static final int MINIMA_CASCADE_LEVELS  = 32;
	
	/**
	 * Current default HASH_Strength Used. Can be up to 512.
	 * All the MINING, TxPoW and MMR data ALWAYS uses 512. But addresses, scripts, and public keys..
	 * can be set to less. This way signatures and addresses are shorter.
	 */
	public static final int MINIMA_DEFAULT_HASH_STRENGTH = 256;
}
