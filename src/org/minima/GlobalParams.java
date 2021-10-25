package org.minima;

import org.minima.objects.base.MiniNumber;

public class GlobalParams {

	/**
	 * Which Version
	 */
	public static String MINIMA_VERSION = "0.99.1";
	
	/**
	 * Number of seconds before sending a pulse message - every 10 minutes
	 */
	public static int USER_PULSE_FREQ   = 10 * 60;
	
	/**
	 * Speed in blocks per second.. 
	 * -  0.05  = 20 second block time
	 */
	public static MiniNumber MINIMA_BLOCK_SPEED  = new MiniNumber("0.02");
	
	/**
	 * When checking speed and average difficulty only look at this many blocks back
	 */
	public static MiniNumber MINIMA_BLOCKS_SPEED_CALC = new MiniNumber(1024);
	
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
	public static MiniNumber MINIMA_CASCADE_START_DEPTH = new MiniNumber(2500);
	
	/**
	 * Number of blocks at each cascade level 
	 */
	public static int MINIMA_CASCADE_LEVEL_NODES  = 128;
	
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
	public static MiniNumber MINIMA_MMR_PROOF_HISTORY = new MiniNumber(256);
	
	/**
	 * Just create a block every transaction. Useful when not mining 
	 * and just want a block every single transaction to debug.
	 * Automatically disables the auto mining
	 */
	public static boolean MINIMA_ZERO_DIFF_BLK  = false;

	/**
	 * Desired number of in link and out links to maintain
	 */
	public static int P2P_NUM_LINKS = 5;

	/**
	 * Desired number of client (nodes that can't accept inbound connections) to maintain
	 */
	public static int P2P_NUM_CLIENT_LINKS = 20;

	/**
	 * Desired number of connections clients should maintain
	 */
	public static int P2P_NUM_CLIENT_CONNECTIONS = 3;

	/**
	 * Time between P2P system assessing its state in milliseconds
	 */
	public static int P2P_LOOP_DELAY = 600_000;

	/**
	 * Max additional ms to add to loop delay (mostly useful during testing to ensure all nodes
	 * aren't perfectly in sync)
	 */
	public static int P2P_LOOP_DELAY_VARIABILITY = 30_000;

	/**
	 * Time between P2P system assessing if it can receive inbound connections milliseconds
	 */
	public static int P2P_NODE_NOT_ACCEPTING_CHECK_DELAY = 3600_000;

	/**
	 * Time we store network mapping info on a node milliseconds
	 */
	public static int P2P_NETWORK_MAP_TTL = 3600_000;

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
	public static int P2P_WALK_LINKS_EXPIRE_TIME = 10_000;

	/**
	 * Time before auth key to expire - required to accept DoSwap and walk based connections
	 */
	public static int P2P_AUTH_KEY_EXPIRY = 300_000;
	
}
