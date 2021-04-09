package org.minima.system.network.base;

import java.util.concurrent.ExecutionException;

// Import log4j classes.
import org.apache.logging.log4j.Logger;
import org.minima.system.network.base.peer.Peer;
import org.apache.logging.log4j.LogManager;

public class P2PStart {


    private static final Logger logger = LogManager.getLogger(P2PStart.class);

    public static void main(String[] args) 
             throws ExecutionException, InterruptedException {
        System.out.println("Hello world!");
        // attempt 1: start with DiscoveryNetworkFactory
        DiscoveryNetworkFactory factory = new DiscoveryNetworkFactory();
        try {
            final DiscoveryNetwork<Peer> network1 = factory.builder().buildAndStart();
            System.out.println("network1 node address: " + network1.getNodeAddress());
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        // attempt 2: start with Eth2P2PNetworkFactory and Eth2P2PNetwork


    }

}

