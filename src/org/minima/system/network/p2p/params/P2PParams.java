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

    public static String METRICS_URL = "http://35.242.245.96/network";

    public static List<InetSocketAddress> DEFAULT_NODE_LIST = Arrays.asList(
            new InetSocketAddress("35.197.193.186", 9001),
            new InetSocketAddress("35.230.131.241", 9001),
            new InetSocketAddress("35.246.66.209", 9001),
            new InetSocketAddress("34.142.20.110", 9001),
            new InetSocketAddress("34.89.91.159", 9001),
            new InetSocketAddress("34.142.19.82", 9001),
            new InetSocketAddress("34.142.47.222", 9001),
            new InetSocketAddress("34.89.75.67", 9001),
            new InetSocketAddress("34.89.23.209", 9001),
            new InetSocketAddress("35.197.193.30", 9001),
            new InetSocketAddress("34.142.123.158", 9001),
            new InetSocketAddress("34.105.136.59", 9001),
            new InetSocketAddress("34.89.101.157", 9001),
            new InetSocketAddress("35.242.183.219", 9001),
            new InetSocketAddress("35.230.134.58", 9001),
            new InetSocketAddress("35.230.157.43", 9001)
            );
    
//    public static List<InetSocketAddress> DEFAULT_NODE_LIST = Arrays.asList();
}
