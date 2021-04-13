package org.minima.system.network.base;

import java.util.Optional;
import java.util.concurrent.ExecutionException;

// Import log4j classes.
import org.apache.logging.log4j.Logger;
import org.minima.system.network.base.peer.LibP2PNodeId;
import org.minima.system.network.base.peer.NodeId;
import org.minima.system.network.base.peer.Peer;

import io.libp2p.core.PeerId;
import io.libp2p.etc.encode.Base58;

import org.apache.logging.log4j.LogManager;

public class P2PStart {


    private static final Logger logger = LogManager.getLogger(P2PStart.class);

    public static void main(String[] args) 
             throws ExecutionException, InterruptedException {
        System.out.println("Hello world!");
        // attempt 1: start with DiscoveryNetworkFactory
        DiscoveryNetworkFactory factory = new DiscoveryNetworkFactory();
        try {
            final DiscoveryNetwork<Peer> network;
            if (args.length == 1) { // first is p2p addr
                     System.out.println("Found addr: " + args[0]);
                     String[] node1_addr_fields = args[0].split("/");
                     String node1_id = node1_addr_fields[node1_addr_fields.length-1];

                     System.out.println("Found node1_id: " + node1_id);
                     // Multiaddr address = Multiaddr.fromString(args[0]);
                     network = factory.builder().staticPeer(args[0]).buildAndStart();
                     //network.getEnr()
                     //network = factory.builder().bootnode(args[0])
                     Thread.sleep(5000);
                     //Peer peer = network.getPeer(args[0]);
                     int peerCount = network.getPeerCount();
                     String addr = network.getNodeAddress();
                     NodeId id = network.getNodeId();
                     System.out.println("peerCount = " + peerCount);
                     System.out.println("addr = " + addr);
                     System.out.println("id = " + id.toString());
                     //discoveryNetworkFactory.builder().bootnode(network1.getEnr().orElseThrow()).buildAndStart();

                     LibP2PNodeId idfirst = new LibP2PNodeId(PeerId.fromBase58(node1_id));
                     Thread.sleep(5000);
                     Waiter.waitFor(
                        () -> {
                            Optional<Peer> firstNode = network.getPeer(idfirst);
                            if(firstNode.isPresent()) {
                                System.out.println("Success! We found the first node.");
                            } else {
                               System.out.println("Not yet there.");
                            }
                            System.out.println("peerCount = " + peerCount);

                        });
                
                    
            } else {
                System.out.println("Creating new address...");
                network = factory.builder().buildAndStart();
                NodeId id = network.getNodeId();
                System.out.println("id = " + id.toString());
            }
            System.out.println("network node address: " + network.getNodeAddress());
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        // attempt 2: start with Eth2P2PNetworkFactory and Eth2P2PNetwork


    }

}

