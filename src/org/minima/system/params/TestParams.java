package org.minima.system.params;

import org.minima.objects.base.MiniNumber;

public class TestParams {

	/**
	 * Which Version
	 */
	public static String MINIMA_VERSION = GlobalParams.MINIMA_VERSION+"-TEST"; 
	
	/**
	 * Speed in blocks per second.. 
	 * -  0.05  = 20 second block time
	 * - 0.2 = 5 second blocktime
	 */
	public static MiniNumber MINIMA_BLOCK_SPEED = new MiniNumber("0.2");
	
	/**
	 * When checking speed and average difficulty only look at this many blocks back
	 */
	public static MiniNumber MINIMA_BLOCKS_SPEED_CALC = new MiniNumber(8);
	
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
	public static MiniNumber MINIMA_CASCADE_START_DEPTH = new MiniNumber(16);
	
	
	/**
	 * Number of blocks at each cascade level 
	 */
	public static int MINIMA_CASCADE_LEVEL_NODES  = 2;
	
	/**
	 * How Many Cascade Levels
	 */
	public static int MINIMA_CASCADE_LEVELS  = 32;
	
	/**
	 * Current default HASH_Strength Used. Can be up to 512.
	 * All the MINING, TxPoW and MMR data ALWAYS uses 512. But addresses, scripts, and public keys..
	 * can be set to less. This way signatures and addresses are shorter.
	 */
	public static int MINIMA_DEFAULT_HASH_STRENGTH = 256;
	
	/**
	 * Max Proof History - how far back to use a proof of coin..
	 * If there is a re-org of more than this the proof will be invalid 
	 */
	public static MiniNumber MINIMA_MMR_PROOF_HISTORY = new MiniNumber(8);

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
		GlobalParams.MINIMA_DEFAULT_HASH_STRENGTH   = TestParams.MINIMA_DEFAULT_HASH_STRENGTH;
		GlobalParams.MINIMA_MMR_PROOF_HISTORY		= TestParams.MINIMA_MMR_PROOF_HISTORY;
		GlobalParams.MINIMA_VERSION					= TestParams.MINIMA_VERSION;
	}
	
}
