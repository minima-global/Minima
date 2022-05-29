package org.minima.system.params;

import org.minima.objects.base.MiniNumber;

public class GlobalParams {

	/**
	 * Which Version
	 */
	public static String MINIMA_VERSION = "0.102.7";
	
	/**
	 * Speed in blocks per second.. 
	 * 0.02 = 50 second block time
	 */
	public static MiniNumber MINIMA_BLOCK_SPEED  = new MiniNumber("0.02");
	
	/**
	 * When checking speed and difficulty only look at this many blocks back
	 */
	public static MiniNumber MINIMA_BLOCKS_SPEED_CALC = new MiniNumber(256);
	
	/**
	 * How deep before we think confirmed..
	 */
	public static MiniNumber MINIMA_CONFIRM_DEPTH  = new MiniNumber("3");
	
	/**
	 * How often do we cascade the chain
	 */
	public static MiniNumber MINIMA_CASCADE_FREQUENCY = new MiniNumber(100);
	
	/**
	 * Depth before we cascade..
	 */
	public static MiniNumber MINIMA_CASCADE_START_DEPTH = new MiniNumber(1024);
	
	/**
	 * Number of blocks at each cascade level 
	 */
	public static int MINIMA_CASCADE_LEVEL_NODES  = 128;
	
	/**
	 * How Many Cascade Levels
	 */
	public static int MINIMA_CASCADE_LEVELS  = 32;
	
	/**
	 * Max Proof History - how far back to use a proof of coin..
	 * If there is a re-org of more than this the proof will be invalid 
	 */
	public static MiniNumber MINIMA_MMR_PROOF_HISTORY = new MiniNumber(256);
	
	/**
	 * The MEDIAN time block is taken from this many blocks back
	 * When calculating the Difficulty of a block ( both from the tip and the previous block )
	 * This smooths out the time fluctuations for different blocks and removes incorrect times.
	 * 
	 * 64 blocks means the block 1/2 hour ago.
	 */
	public static int MEDIAN_BLOCK_CALC = 64; 
	
}
