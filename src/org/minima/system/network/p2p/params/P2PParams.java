package org.minima.system.network.p2p.params;

import java.net.InetSocketAddress;
import java.util.Arrays;
import java.util.List;

import org.minima.system.network.p2p.P2PFunctions;

public class P2PParams {

    // P2P Version number (Major - breaking changes. Minor - none breaking changes)
    public static String VERSION = "1.1";

    /**
     * P2P Log level
     */
    public static P2PFunctions.Level LOG_LEVEL = P2PFunctions.Level.NODE_RUNNER_MSG;
    /**
     * Max number of peers to keep in the peers list
     */
    public static int PEERS_LIST_SIZE = 50;

    /**
     * Desired number of in link and out links to maintain
     */
    public static int TGT_NUM_LINKS = 5;

    /**
     * Desired number of client (nodes that can't accept inbound connections) to maintain
     */
    public static int TGT_NUM_NONE_P2P_LINKS = 100;

    /**
     * Desired number of connections clients should maintain
     */
    public static int MIN_NUM_CONNECTIONS = 2;

    /**
     * Time between P2P system assessing its state in milliseconds
     */
    public static int LOOP_DELAY = 600_000;

    /**
     * Time between updating the device hash_rate in milliseconds
     */
    //                                         H    M    S    millis
    public static int HASH_RATE_UPDATE_DELAY = 12 * 60 * 60 * 1000;

    /**
     * Max additional ms to add to loop delay (mostly useful during testing to ensure all nodes
     * aren't perfectly in sync)
     */
    public static int LOOP_DELAY_VARIABILITY = 30_000;

    /**
     * Time between P2P system assessing if it can receive inbound connections milliseconds
     */
    public static int NODE_NOT_ACCEPTING_CHECK_DELAY = 3600_000;

    public static int SAVE_DATA_DELAY = 3600_000;

    /**
     * Time in ms before walk link messages expire
     */
    public static int WALK_LINKS_EXPIRE_TIME = 10_000;

    /**
     * Time before auth key to expire - required to accept DoSwap and walk based connections
     */
    public static int AUTH_KEY_EXPIRY = 300_000;

    public static int METRICS_DELAY = 600_000;

    //NOT USED
    public static String METRICS_URL = "http://35.242.245.96/network";

//    public static List<InetSocketAddress> DEFAULT_NODE_LIST = Arrays.asList(
//            
//    		new InetSocketAddress("35.185.184.164", 9001),
//            new InetSocketAddress("35.197.193.30", 9001),
//            new InetSocketAddress("34.142.123.158", 9001),
//            new InetSocketAddress("34.89.23.209", 9001),
//            new InetSocketAddress("34.135.149.22", 9001),
//            
//            new InetSocketAddress("34.28.118.235", 9001),
//            new InetSocketAddress("34.171.105.59", 9001),
//            new InetSocketAddress("34.122.109.104", 9001),
//            new InetSocketAddress("34.142.197.222", 9001),
//            new InetSocketAddress("34.124.175.150", 9001),
//            
//            new InetSocketAddress("35.198.247.214", 9001),
//            new InetSocketAddress("34.165.238.9", 9001),
//            new InetSocketAddress("34.165.93.170", 9001),
//            new InetSocketAddress("34.165.41.114", 9001),
//            new InetSocketAddress("34.165.177.125", 9001),
//            
//            new InetSocketAddress("34.142.47.222", 9001)
//            );
    
    public static List<InetSocketAddress> DEFAULT_NODE_LIST = Arrays.asList();
    
//    public static List<InetSocketAddress> DEFAULT_ARCHIVENODE_LIST = Arrays.asList();
//    public static List<InetSocketAddress> DEFAULT_ARCHIVENODE_LIST = Arrays.asList(
//            
//    		new InetSocketAddress("34.124.218.35", 9001),
//            new InetSocketAddress("34.165.40.140", 9001),
//            new InetSocketAddress("35.246.125.190", 9001),
//            new InetSocketAddress("34.135.253.11", 9001),
//            
//            new InetSocketAddress("35.200.57.74", 9001),
//            new InetSocketAddress("35.189.7.111", 9001),
//            new InetSocketAddress("34.65.2.203", 9001),
//            new InetSocketAddress("34.82.99.198", 9001)
//            
//    		);

}
