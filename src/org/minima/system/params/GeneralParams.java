package org.minima.system.params;

import org.minima.objects.base.MiniNumber;

public class GeneralParams {

	/**
	 * Are you running on a mobile
	 */
	public static boolean IS_MOBILE = false;
	
	/**
	 * Are you running on a desktop
	 */
	public static boolean IS_DESKTOP = false;
	
	/**
	 * Are you running on a desktop
	 */
	public static boolean IS_JNLP = false;
	
	/**
	 * Can you accept incoming connections.. 
	 */
	public static boolean IS_ACCEPTING_IN_LINKS = true;
	
	/**
	 * Runs a private network..
	 * 
	 *  If no configuration files are found with use -genesis
	 *  If configuration files are found wil not use -genesis
	 *  
	 *  Also adds -test and -nop2p
	 */
	public static boolean PRIVATE = false;
	
	/**
	 * Are we creating the genesis block 
	 */
	public static boolean GENESIS = false;
	
	/**
	 * Are we wiping previous data 
	 */
	public static boolean CLEAN = false;
	
	/**
	 * Are we running an Archive node
	 */
	public static boolean ARCHIVE = false;
	
	/**
	 * Are we running an TxBlock ONLY node
	 */
	public static boolean TXBLOCK_NODE = false;
	
	/**
	 * The SQL DB Aes Password. If it is 'minima' no Encryption used
	 */
	public static boolean IS_MAIN_DBPASSWORD_SET = false;
	public static String MAIN_DBPASSWORD 		 = "minima"; 
	
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
	 * Host and IP of the first P2P node..
	 */
	public static String P2P_ADDNODES = "";
	
	/**
	 * Is the P2P2 System Enabled
	 */
	public static boolean P2P2_ENABLED = false;
	
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
	 * Maximumum output Coins allowed in txn for relay policy
	 */
	public static int MAX_RELAY_OUTPUTCOINS = 32;
	
	/**
	 * Maximumum state size store for ALL coins for relay policy - 64K
	 */
	public static long MAX_RELAY_STORESTATESIZE = 65536;
	
	/**
	 * Max Split coins
	 */
	public static MiniNumber MAX_SPLIT_COINS = new MiniNumber(20);
	
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
	public static long NUMBER_DAYS_ARCHIVE = 50;
	
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
	
	/**
	 * Enable full logs for Maxima..
	 */
	public static boolean MAXIMA_LOGS = false;
	
	/**
	 * Enable full logs for Networking..
	 */
	public static boolean NETWORKING_LOGS = false;
	
	/**
	 * Enable full logs for IBD SYNC..
	 */
	public static boolean IBDSYNC_LOGS = false;
	
	/**
	 * Enable full logs for Block Info..
	 */
	public static boolean BLOCK_LOGS = false;
	
	/**
	 * Do we LIMIT the bandwidth (-mobile)
	 */
	public static boolean ARCHIVESYNC_LIMIT_BANDWIDTH = false;
	
	/**
	 * NODEJS requires the RPC command to be CRLF
	 */
	public static boolean RPC_CRLF = false;
	
	/**
	 * DO we install the Default DAPPS
	 */
	public static boolean DEFAULT_MINIDAPPS = true;
	
	/**
	 * Authentication for RPC
	 */
	public static boolean RPC_ENABLED 		= false;
	public static boolean RPC_AUTHENTICATE 	= false;
	public static boolean RPC_SSL			= false;
	public static String RPC_AUTHSTYLE 		= "basic";
	public static String RPC_PASSWORD 		= "none";
	
	/**
	 * Specify seed phrase on launch!
	 */
	public static String SEED_PHRASE = ""; 
	
	/**
	 * Do we log peers checker messages
	 */
	public static boolean PEERSCHECKER_lOG = false;
	
	/**
	 * Are we running a MEGA MMR
	 */
	public static boolean IS_MEGAMMR = false;
	
	/**
	 * Notify on ALL TxPoW whether relevant or not
	 */
	public static boolean NOTIFY_ALL_TXPOW = false;
	
	/**
	 * Do we MegaMMR sync on Heavier Chain
	 */
	public static String RESCUE_MEGAMMR_NODE 	= "";
	
	/**
	 * Do we use use HTTP instead of self signed HTTPS for MDS
	 */
	public static boolean MDS_NOSSL = false;
	
	/**
	 * Setup MySQL DB from command line
	 */
	public static String MYSQL_DB_DETAILS = "";
	
	/**
	 * Is there a delay before attempting a MySQL connection (docker can take 30 seconds to start mysql container)
	 */
	public static int MYSQL_DB_DELAY = 0;
	
	/**
	 * Is there a delay before attempting a MySQL connection (docker can take 30 seconds to start mysql container)
	 */
	public static boolean MYSQL_DB_COINS = false;
	
	/**
	 * Do we store all txpow with MySQL Autobackup
	 */
	public static boolean MYSQL_STORE_ALLTXPOW = false;
	
	/**
	 * Enable the Public MDS from command line
	 */
	public static boolean PUBLICMDS_ENABLE = false;
	
	/**
	 * Hard set the Public MDS UID
	 */
	public static String PUBLICMDS_SESSION_UID = "";
	
	/**
	 * Do we prune The MegaMMR
	 */
	public static boolean MEGAMMR_MEGAPRUNE = false;
	
	/**
	 * Do we prune all addresses with a state (useful for exchanges that don't need them)
	 */
	public static boolean MEGAMMR_MEGAPRUNE_STATE = false;
	
	/**
	 * Do we prune all tokens - only Minima allowed
	 */
	public static boolean MEGAMMR_MEGAPRUNE_TOKENS = false;
	
	/**
	 * Do we show the network requests
	 */
	public static boolean SHOW_NETWORK_CALLS = false;
	
	/**
	 * IF dhownet calls do we show POLL
	 */
	public static boolean SHOW_NETWORK_POLLS = true;
	
	/**
	 * Reset all params to their default settings.. ANDROID keeps them after shutdown
	 */
	public static void resetDefaults() {
		IS_MOBILE = false;
		IS_DESKTOP = false;
		IS_JNLP = false;
		IS_ACCEPTING_IN_LINKS = true;
		GENESIS = false;
		PRIVATE = false;
		CLEAN = false;
		ARCHIVE = false;
		TXBLOCK_NODE = false;
		IS_MAIN_DBPASSWORD_SET = false;
		MAIN_DBPASSWORD 		 = "minima"; 
		DATA_FOLDER = "";
		BASE_FILE_FOLDER = "";
		MINIMA_HOST = "";
		IS_HOST_SET = false;
		MINIMA_PORT = 9001;
		MDSFILE_PORT = MINIMA_PORT+2;
		MDSCOMMAND_PORT = MINIMA_PORT+3;
		RPC_PORT = MINIMA_PORT+4;
		MDS_PASSWORD = "";
		MDS_INITFOLDER = "";
		MDS_WRITE = "";
		MDS_ENABLED = false;
		TEST_PARAMS = false;
		P2P_ENABLED = true;
		P2P_ROOTNODE = "";
		P2P_ADDNODES = "";
		ALLOW_ALL_IP = false;
		NOCONNECT = false;
		CONNECT_LIST = "";
		SHOW_PARAMS = false;
		NO_SYNC_IBD = false;
		MAX_RELAY_OUTPUTCOINS = 15;
		MAX_SPLIT_COINS = new MiniNumber(10);
		NUMBER_DAYS_SQLTXPOWDB = 3;
		NUMBER_HOURS_RAMTXPOWDB = 1;
		NUMBER_DAYS_ARCHIVE = 50;
		USER_PULSE_FREQ   = 1000 * 60 * 10;
		DEBUGFLAG = false;
		DEBUGVAR 	= "";
		SCRIPTLOGS = false;
		MINING_LOGS = false;
		MAXIMA_LOGS = false;
		NETWORKING_LOGS = false;
		IBDSYNC_LOGS = false;
		BLOCK_LOGS = false;
		ARCHIVESYNC_LIMIT_BANDWIDTH = false;
		RPC_CRLF = false;
		DEFAULT_MINIDAPPS 	  = true;
		RPC_ENABLED 		  =  false;
		RPC_AUTHENTICATE 	  = false;
		RPC_SSL				  = false;
		RPC_AUTHSTYLE 		  = "basic";
		RPC_PASSWORD 		  = "none";
		SEED_PHRASE 		  = "";
		PEERSCHECKER_lOG 	  = false;
		IS_MEGAMMR 			  = false;
		NOTIFY_ALL_TXPOW 	  = false;
		RESCUE_MEGAMMR_NODE	  = "";
		MDS_NOSSL 			  = false;
		
		MYSQL_STORE_ALLTXPOW  	= false;
		MYSQL_DB_DETAILS 		= "";
		MYSQL_DB_DELAY 			= 0;
		MYSQL_DB_COINS 			= false;
		
		PUBLICMDS_ENABLE 		= false;
		PUBLICMDS_SESSION_UID 	= "";
		
		MEGAMMR_MEGAPRUNE 	  		= false;
		MEGAMMR_MEGAPRUNE_STATE 	= false;
		MEGAMMR_MEGAPRUNE_TOKENS	= false;
		
		P2P2_ENABLED 		  	= false;
		SHOW_NETWORK_CALLS 		= false;
	}
	
}
