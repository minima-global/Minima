package org.minima.system.params;

public class GeneralParams {

	/**
	 * Are you running on a mobile - mainly for metrics
	 */
	public static boolean IS_MOBILE = false;
	
	/**
	 * Can you accpet incoming connections.. 
	 */
	public static boolean IS_ACCEPTING_IN_LINKS = true;
	
//	/**
//	 * Is this a private network - don;t connect to any users..
//	 */
//	public static boolean PRIVATE_NETWORK = false;
//	
//	/**
//	 * Are we automining a TxPoW every block
//	 */
//	public static boolean AUTOMINE = false;
//	
	/**
	 * Are we creating the genesis block 
	 */
	public static boolean GENESIS = false;
	
	/**
	 * Are we wiping previous data 
	 */
	public static boolean CLEAN = false;
	
	/**
	 * Where are the database files stored. This is set at startup.
	 */
	public static String DATA_FOLDER = "";
	
	/**
	 * The Host IP
	 */
	public static String MINIMA_HOST = "";
	
	/**
	 * Is the HOST set from command line
	 */
	public static boolean IS_HOST_SET = false;
	
	/**
	 * The main Minima port
	 */
	public static int MINIMA_PORT = 9001;
	
	/**
	 * The Minima RPC port
	 */
	public static int RPC_PORT = 9002;
	
	/**
	 * Test Params or Main Params
	 */
	public static boolean TEST_PARAMS = false;
	
	/**
	 * Is the P2P System Enabled
	 */
	public static boolean P2P_ENABLED = true;
	
	/**
	 * Host and IP of the first P2P node..
	 */
	public static String P2P_ROOTNODE = "";
	
	/**
	 * Manual list of Minima nodes to connect to
	 */
	public static boolean NOCONNECT = false;
	
	/**
	 * Manual list of Minima nodes to connect to
	 */
	public static String CONNECT_LIST = "";
	
	/**
	 * How many days do you keep the TxPoW in the SQL DB
	 */
	public static long NUMBER_DAYS_SQLTXPOWDB = 3;
	
	/**
	 * How Many Hours do you keep the TxPOW in the RAM mempool
	 */
	public static long NUMBER_HOURS_RAMTXPOWDB = 1;
	
	/**
	 * How many days do you archive the TxBlocks to resync Users
	 */
	public static long NUMBER_DAYS_ARCHIVE = 90;
	
	/**
	 * Number of seconds before sending a pulse message - every 10 minutes
	 */
	public static long USER_PULSE_FREQ   = 1000 * 60 * 10;
	
	/**
	 * MY Debug Func flag - for testing.. can set with debugfunc
	 */
	public static boolean DEBUGFLAG = false;
	public static String DEBUGVAR 	= "";
	
}
