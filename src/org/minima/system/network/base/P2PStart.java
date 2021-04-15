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
            String node1_id;
            String[] node1_addr_fields;
            String node2_id;
            String[] node2_addr_fields;
            final int peerCount;
            String addr;
            NodeId id;

            if (args.length == 2) { // first is p2p addr
                System.out.println("Found addr node 1: " + args[0]);
                System.out.println("Found addr node 2: " + args[1]);
                node1_addr_fields = args[0].split("/");
                node1_id = node1_addr_fields[node1_addr_fields.length-1];
                node2_addr_fields = args[1].split("/");
                node2_id = node2_addr_fields[node2_addr_fields.length-1];
                System.out.println("Found node2_id: " + node1_id);
                // Multiaddr address = Multiaddr.fromString(args[0]);
                network = factory.builder().staticPeer(args[0]).buildAndStart();
                //network.getEnr()
                //network = factory.builder().bootnode(args[0])
                Thread.sleep(5000);
                //Peer peer = network.getPeer(args[0]);
                peerCount = network.getPeerCount();
                addr = network.getNodeAddress();
                id = network.getNodeId();
                System.out.println("peerCount = " + peerCount);
                System.out.println("addr = " + addr);
                System.out.println("id = " + id.toString());
                //discoveryNetworkFactory.builder().bootnode(network1.getEnr().orElseThrow()).buildAndStart();

                LibP2PNodeId id_1 = new LibP2PNodeId(PeerId.fromBase58(node1_id));
                LibP2PNodeId id_2 = new LibP2PNodeId(PeerId.fromBase58(node2_id));
                //Thread.sleep(5000);
                Waiter.waitFor(
                   () -> {
                       Optional<Peer> firstNode = network.getPeer(id_1);
                       if(firstNode.isPresent()) {
                           System.out.println("Success! We found the first node: " + firstNode.get().getAddress());
                       } else {
                          System.out.println("First node not found.");
                       }
                       Optional<Peer> secondNode = network.getPeer(id_2);
                       if(secondNode.isPresent()) {
                        System.out.println("Success! We found the second node: " + secondNode.get().getAddress());
                    } else {
                       System.out.println("Second node not found.");
                    }
                       System.out.println("peerCount = " + peerCount);
                });
            } else if (args.length == 1) { // first is p2p addr
                     System.out.println("Found addr: " + args[0]);
                     node1_addr_fields = args[0].split("/");
                     node1_id = node1_addr_fields[node1_addr_fields.length-1];

                     System.out.println("Found node1_id: " + node1_id);
                     // Multiaddr address = Multiaddr.fromString(args[0]);
                     network = factory.builder().staticPeer(args[0]).buildAndStart();
                     //network.getEnr()
                     //network = factory.builder().bootnode(args[0])
                     Thread.sleep(1000);
                     //Peer peer = network.getPeer(args[0]);
                     peerCount = network.getPeerCount();
                     addr = network.getNodeAddress();
                     id = network.getNodeId();
                     System.out.println("peerCount = " + peerCount);
                     System.out.println("addr = " + addr);
                     System.out.println("id = " + id.toString());
                     //discoveryNetworkFactory.builder().bootnode(network1.getEnr().orElseThrow()).buildAndStart();

                     LibP2PNodeId idfirst = new LibP2PNodeId(PeerId.fromBase58(node1_id));
                     //Thread.sleep(5000);
                     Waiter.waitFor(
                        () -> {
                            Optional<Peer> firstNode = network.getPeer(idfirst);
                            if(firstNode.isPresent()) {
                                System.out.println("Success! We found the first node: " + firstNode.get().getAddress());
                            } else {
                               System.out.println("First node not found.");
                            }
                            System.out.println("peerCount = " + peerCount);

                        });    
            } else {
                System.out.println("Creating new address...");
                network = factory.builder().buildAndStart();
                id = network.getNodeId();
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

