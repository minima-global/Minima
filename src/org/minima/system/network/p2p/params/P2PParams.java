package org.minima.system.network.p2p.params;

import java.net.InetSocketAddress;
import java.util.Arrays;
import java.util.List;

public class P2PParams {

    // P2P Version number (Major - breaking changes. Minor - none breaking changes)
    public static String VERSION = "1.0";

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

    public static String METRICS_URL = "http://35.187.82.116/network";

    public static List<InetSocketAddress> DEFAULT_NODE_LIST = Arrays.asList(
            new InetSocketAddress("35.189.237.10", 9001), // v101-genesis-node
            new InetSocketAddress("34.92.63.200", 9001), // v101-asia-hong-kong
            new InetSocketAddress("34.93.179.55", 9001), // v101-asia-india
            new InetSocketAddress("34.65.164.213", 9001), // v101-eu-zurich
            new InetSocketAddress("34.151.221.133", 9001), // v101-southamerica-east
            new InetSocketAddress("34.67.254.187", 9001) // v101-usa-central
            );

}
