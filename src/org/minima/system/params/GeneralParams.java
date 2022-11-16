package org.minima.system.params;

public class GeneralParams {

	/**
	 * Are you running on a mobile - mainly for metrics
	 */
	public static boolean IS_MOBILE = false;
	
	/**
	 * Can you accept incoming connections.. 
	 */
	public static boolean IS_ACCEPTING_IN_LINKS = true;
	
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
	 * Default backup / restore / file creation  folder
	 */
	public static String BASE_FILE_FOLDER = "";
	
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
	 * The MDS webserver
	 */
	public static int MDSFILE_PORT = MINIMA_PORT+2;
	
	/**
	 * The MDS Command Server
	 */
	public static int MDSCOMMAND_PORT = MINIMA_PORT+3;
	
	/**
	 * The Minima RPC port
	 */
	public static int RPC_PORT = MINIMA_PORT+4;
	
	/**
	 * The MDS Password - blank will set automagically
	 */
	public static String MDS_PASSWORD = "";
	
	/**
	 * A folder of MDS dapps to install at startup
	 */
	public static String MDS_INITFOLDER = "";
	
	/**
	 * Make a MiniDAPP WRITE access - on init install
	 */
	public static String MDS_WRITE = "";
	
	/**
	 * MDS ENabled
	 */
	public static boolean MDS_ENABLED = false;
	
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
	 * Are all IP allowed for Maxima Hosts
	 */
	public static boolean ALLOW_ALL_IP = false;
	
	/**
	 * Manual list of Minima nodes to connect to
	 */
	public static boolean NOCONNECT = false;
	
	/**
	 * Manual list of Minima nodes to connect to
	 */
	public static String CONNECT_LIST = "";
	
	/**
	 * Show the startup parameters
	 */
	public static boolean SHOW_PARAMS = false;
	
	/**
	 * Don't download sync IBD
	 */
	public static boolean NO_SYNC_IBD = false;
	
	/**
	 * MySQL Archive Settings
	 */
	public static String MYSQL_HOST 	= "";
	public static String MYSQL_DB 		= "";
	public static String MYSQL_USER 	= "";
	public static String MYSQL_PASSWORD = "";
	
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
	public static long NUMBER_DAYS_ARCHIVE = 60;
	
	/**
	 * Number of seconds before sending a pulse message - every 10 minutes
	 */
	public static long USER_PULSE_FREQ   = 1000 * 60 * 10;
	
	/**
	 * MY Debug Func flag - for testing.. can set with debugfunc
	 */
	public static boolean DEBUGFLAG = false;
	public static String DEBUGVAR 	= "";
	
	/**
	 * Enable Full logs for script errors
	 */
	public static boolean SCRIPTLOGS = false;
	
	/**
	 * Enable full logs for mining..
	 */
	public static boolean MINING_LOGS = false;
	
}
