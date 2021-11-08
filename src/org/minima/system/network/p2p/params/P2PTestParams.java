package org.minima.system.network.p2p.params;

import java.net.InetSocketAddress;
import java.util.Arrays;
import java.util.List;

public class P2PTestParams {
    
    /**
     * Desired number of in link and out links to maintain
     */
    public static int TGT_NUM_LINKS = 5;

    /**
     * Desired number of client (nodes that can't accept inbound connections) to maintain
     */
    public static int TGT_NUM_NONE_P2P_LINKS = 10;

    /**
     * Desired number of connections clients should maintain
     */
    public static int MIN_NUM_CONNECTIONS = 3;

    /**
     * Time between P2P system assessing its state in milliseconds
     */
    public static int LOOP_DELAY = 5_000;

    /**
     * Max additional ms to add to loop delay (mostly useful during testing to ensure all nodes
     * aren't perfectly in sync)
     */
    public static int LOOP_DELAY_VARIABILITY = 3_000;

    /**
     * Time between P2P system assessing if it can receive inbound connections milliseconds
     */
    public static int NODE_NOT_ACCEPTING_CHECK_DELAY = 600_000;

    /**
     * Time in ms before walk link messages expire
     */
    public static int WALK_LINKS_EXPIRE_TIME = 5_000;

    /**
     * Time before auth key to expire - required to accept DoSwap and walk based connections
     */
    public static int AUTH_KEY_EXPIRY = 300_000;


    public static List<InetSocketAddress> DEFAULT_NODE_LIST = Arrays.asList(
            new InetSocketAddress("127.0.0.1", 9001),
            new InetSocketAddress("127.0.0.1", 9101)
    );

    public static void setTestParams() {
        P2PParams.DEFAULT_NODE_LIST = DEFAULT_NODE_LIST;
        P2PParams.LOOP_DELAY = LOOP_DELAY;
        P2PParams.LOOP_DELAY_VARIABILITY = LOOP_DELAY_VARIABILITY;
        P2PParams.NODE_NOT_ACCEPTING_CHECK_DELAY = NODE_NOT_ACCEPTING_CHECK_DELAY;
        P2PParams.WALK_LINKS_EXPIRE_TIME = WALK_LINKS_EXPIRE_TIME;
        P2PParams.AUTH_KEY_EXPIRY = AUTH_KEY_EXPIRY;
        P2PParams.TGT_NUM_LINKS = TGT_NUM_LINKS;
        P2PParams.TGT_NUM_NONE_P2P_LINKS = TGT_NUM_NONE_P2P_LINKS;
        P2PParams.MIN_NUM_CONNECTIONS = MIN_NUM_CONNECTIONS;
    }


}
