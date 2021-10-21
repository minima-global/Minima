package org.minima;

import org.minima.objects.base.MiniNumber;

public class TestParams {

	/**
	 * Which Version
	 */
	public static String MINIMA_VERSION = GlobalParams.MINIMA_VERSION+"-TEST"; 
	
	/**
	 * Number of seconds before sending a pulse message - every 10 minutes
	 */
	public static int USER_PULSE_FREQ   = 10 * 60;
	
	/**
	 * Speed in blocks per second.. 
	 * -  0.05  = 20 second block time
	 * - 0.2 = 5 second blocktime
	 */
	public static MiniNumber MINIMA_BLOCK_SPEED  = new MiniNumber("0.5");
	
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
	public static MiniNumber MINIMA_CASCADE_FREQUENCY = new MiniNumber(5);
	
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
	public static int MINIMA_DEFAULT_HASH_STRENGTH = 160;
	
	/**
	 * Max Proof History - how far back to use a proof of coin..
	 * If there is a re-org of more than this the proof will be invalid 
	 */
	public static MiniNumber MINIMA_MMR_PROOF_HISTORY = new MiniNumber(5);
	
	/**
	 * Just create a block every transaction. Useful when not mining 
	 * and just want a block every single transaction to debug.
	 * Automatically disables the auto mining
	 */
	public static boolean MINIMA_ZERO_DIFF_BLK  = false;

	/**
	 * Time between P2P system assessing its state in milliseconds
	 */
	public static int P2P_LOOP_DELAY = 10_000;

	/**
	 * Max additional ms to add to loop delay (mostly useful during testing to ensure all nodes
	 * aren't perfectly in sync)
	 */
	public static int P2P_LOOP_DELAY_VARIABILITY = 5_000;

	/**
	 * Time between P2P system assessing if it can receive inbound connections milliseconds
	 */
	public static int P2P_NODE_NOT_ACCEPTING_CHECK_DELAY = 300_000;

	/**
	 * Time we store network mapping info on a node milliseconds
	 */
	public static int P2P_NETWORK_MAP_TTL = 10_000;

	/**
	 * Max number of nodes to store in memory
	 */
	public static int P2P_MAX_NETWORK_MAP_SIZE = 10_000;

	/**
	 * Max time in ms before we return the network map (needs to be less than the global timeout hard coded to 30s)
	 */
	public static int P2P_MAX_NETWORK_MAP_RESPONSE_TIME = 20_000;

	/**
	 * Time in ms before walk link messages expire
	 */
	public static int P2P_WALK_LINKS_EXPIRE_TIME = 5_000;

	/**
	 * Time before auth key to expire - required to accept DoSwap and walk based connections
	 */
	public static int P2P_AUTH_KEY_EXPIRY = 300_000;


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
		GlobalParams.MINIMA_ZERO_DIFF_BLK			= TestParams.MINIMA_ZERO_DIFF_BLK;
		GlobalParams.P2P_LOOP_DELAY = TestParams.P2P_LOOP_DELAY;
		GlobalParams.P2P_LOOP_DELAY_VARIABILITY = TestParams.P2P_LOOP_DELAY_VARIABILITY;
		GlobalParams.P2P_NODE_NOT_ACCEPTING_CHECK_DELAY = TestParams.P2P_NODE_NOT_ACCEPTING_CHECK_DELAY;
		GlobalParams.P2P_NETWORK_MAP_TTL = TestParams.P2P_NETWORK_MAP_TTL;
		GlobalParams.P2P_MAX_NETWORK_MAP_SIZE = TestParams.P2P_MAX_NETWORK_MAP_SIZE;
		GlobalParams.P2P_MAX_NETWORK_MAP_RESPONSE_TIME = TestParams.P2P_MAX_NETWORK_MAP_RESPONSE_TIME;
		GlobalParams.P2P_WALK_LINKS_EXPIRE_TIME = TestParams.P2P_WALK_LINKS_EXPIRE_TIME;
		GlobalParams.P2P_AUTH_KEY_EXPIRY = TestParams.P2P_AUTH_KEY_EXPIRY;
	}
	
}
