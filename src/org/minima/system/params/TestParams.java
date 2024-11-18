package org.minima.system.params;

import org.minima.objects.base.MiniNumber;

public class TestParams {

	/**
	 * Which Version
	 */
	public static String MINIMA_VERSION = GlobalParams.MINIMA_VERSION+"-TEST"; 
	
	/**
	 * Speed in blocks per second.. 
	 * -  0.02  = 50 second block time
	 * -  0.05  = 20 second block time
	 * -  0.2   =  5 second block time
	 */
	public static MiniNumber MINIMA_BLOCK_SPEED = new MiniNumber("0.05");
	
	/**
	 * When checking speed and average difficulty only look at this many blocks back
	 */
	public static MiniNumber MINIMA_BLOCKS_SPEED_CALC = new MiniNumber(16);
	
	/**
	 * How deep before we think confirmed..
	 */
	public static MiniNumber MINIMA_CONFIRM_DEPTH  = new MiniNumber("3");
	
	/**
	 * How often do we cascade the chain
	 */
	public static MiniNumber MINIMA_CASCADE_FREQUENCY = new MiniNumber(3);
	
	/**
	 * Depth before we cascade..
	 */
	public static MiniNumber MINIMA_CASCADE_START_DEPTH = new MiniNumber(32);
	
	
	/**
	 * Number of blocks at each cascade level 
	 */
	public static int MINIMA_CASCADE_LEVEL_NODES  = 4;
	
	/**
	 * How Many Cascade Levels
	 */
	public static int MINIMA_CASCADE_LEVELS  = 32;
	
	/**
	 * Max Proof History - how far back to use a proof of coin..
	 * If there is a re-org of more than this the proof will be invalid 
	 */
	public static MiniNumber MINIMA_MMR_PROOF_HISTORY = new MiniNumber(8);

	/**
	 * The MEDIAN time block is taken from this many blocks back
	 * When calculating the Difficulty of a block ( both from the tip and the previous block )
	 * This smooths out the time fluctuations for different blocks and removes incorrect times.
	 * 
	 * 64 blocks means the block 1/2 hour ago.
	 */
	public static int MEDIAN_BLOCK_CALC = 8;
	
	/**
	 * Set these as the GlobalParams..
	 */
	public static void setTestParams() {
		GlobalParams.MINIMA_BLOCK_SPEED 			= TestParams.MINIMA_BLOCK_SPEED;
		GlobalParams.MINIMA_BLOCKS_SPEED_CALC 		= TestParams.MINIMA_BLOCKS_SPEED_CALC;
		GlobalParams.MINIMA_CASCADE_FREQUENCY 		= TestParams.MINIMA_CASCADE_FREQUENCY;
		GlobalParams.MINIMA_CASCADE_LEVEL_NODES		= TestParams.MINIMA_CASCADE_LEVEL_NODES;
		GlobalParams.MINIMA_CASCADE_LEVELS			= TestParams.MINIMA_CASCADE_LEVELS;
		GlobalParams.MINIMA_CASCADE_START_DEPTH		= TestParams.MINIMA_CASCADE_START_DEPTH;
		GlobalParams.MINIMA_CONFIRM_DEPTH			= TestParams.MINIMA_CONFIRM_DEPTH;
		GlobalParams.MINIMA_MMR_PROOF_HISTORY		= TestParams.MINIMA_MMR_PROOF_HISTORY;
		GlobalParams.MINIMA_VERSION					= TestParams.MINIMA_VERSION;
		GlobalParams.MEDIAN_BLOCK_CALC				= TestParams.MEDIAN_BLOCK_CALC;
	}	
}
