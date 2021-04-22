package org.minima.system.network.base;

import java.net.InetSocketAddress;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.stream.Collectors;

// Import log4j classes.

// Import log4j classes.
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;
import org.ethereum.beacon.discovery.schema.NodeRecord;
import org.ethereum.beacon.discovery.schema.NodeRecordInfo;
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
        final DiscoveryNetwork<Peer> network;
        try {
            
            String node1_id;
            String[] node1_addr_fields;
            String node2_id;
            String[] node2_addr_fields;
            final int peerCount;
            String addr;
            NodeId id;

            if (args.length == 2) { // first is p2p addr, second is enr
                System.out.println("Found addr node 1: " + args[0]);
                System.out.println("Found boot node 1: " + args[1]);
                node1_addr_fields = args[0].split("/");
                node1_id = node1_addr_fields[node1_addr_fields.length-1];
//                node2_addr_fields = args[1].split("/");
//                node2_id = node2_addr_fields[node2_addr_fields.length-1];
//                System.out.println("Found node2_id: " + node1_id);
                String bootnode_1 = args[1];
                // System.out.println("Found boot node addr: " + bootnode_1);
                // Multiaddr address = Multiaddr.fromString(args[0]);
                network = factory.builder().staticPeer(args[0]).bootnode(bootnode_1).buildAndStart();
                //network.getEnr()
                //network = factory.builder().bootnode(args[0])
                //Thread.sleep(5000);
                //Peer peer = network.getPeer(args[0]);
                // peerCount = network.getPeerCount();
                // addr = network.getNodeAddress();
                // id = network.getNodeId();
                // // System.out.println("peerCount = " + peerCount);
                // System.out.println("addr = " + addr);
                // System.out.println("id = " + id.toString());
                //discoveryNetworkFactory.builder().bootnode(network1.getEnr().orElseThrow()).buildAndStart();

                LibP2PNodeId id_1 = new LibP2PNodeId(PeerId.fromBase58(node1_id));

//                LibP2PNodeId id_2 = new LibP2PNodeId(PeerId.fromBase58(node2_id));
                //Thread.sleep(5000);
//                 Waiter.waitFor(
//                    () -> {
//                        Optional<Peer> firstNode = network.getPeer(id_1);
//                        if(firstNode.isPresent()) {
//                            System.out.println("Success! We found the first node: " + firstNode.get().getAddress());
//                        } else {
//                           System.out.println("First node not found.");
//                        }
// //                       Optional<Peer> secondNode = network.getPeer(id_2);
//                     //    if(secondNode.isPresent()) {
//                     //     System.out.println("Success! We found the second node: " + secondNode.get().getAddress());
//                     // } else {
//                     //    System.out.println("Second node not found.");
//                     // }
//                        System.out.println("peerCount = " + peerCount);
//                 });
            } else if (args.length == 1) { // first is p2p addr - deprecated - should start with p2p addr and ENR (application level address)
                    logger.warn("Careful! This mode is deprecated, either start with 0 or 2 args, not 1.");
                    System.out.println("Found addr: " + args[0]);
                     node1_addr_fields = args[0].split("/");
                     node1_id = node1_addr_fields[node1_addr_fields.length-1];

                     System.out.println("Found node1_id: " + node1_id);
                     // Multiaddr address = Multiaddr.fromString(args[0]);
                     network = factory.builder().staticPeer(args[0]).buildAndStart();
                     //network.getEnr()
                     //network = factory.builder().bootnode(args[0])
//                     Thread.sleep(1000);
                     //Peer peer = network.getPeer(args[0]);
                    //  peerCount = network.getPeerCount();
                    //  addr = network.getNodeAddress();
                    //  id = network.getNodeId();
//                     System.out.println("peerCount = " + peerCount);
//                     System.out.println("addr = " + addr);
//                     System.out.println("id = " + id.toString());
                     //discoveryNetworkFactory.builder().bootnode(network1.getEnr().orElseThrow()).buildAndStart();

                     //LibP2PNodeId idfirst = new LibP2PNodeId(PeerId.fromBase58(node1_id));
                     //Thread.sleep(5000);
                    //  Waiter.waitFor(
                    //     () -> {
                    //         Optional<Peer> firstNode = network.getPeer(idfirst);
                    //         if(firstNode.isPresent()) {
                    //             System.out.println("Success! We found the first node: " + firstNode.get().getAddress());
                    //         } else {
                    //            System.out.println("First node not found.");
                    //         }
                    //         System.out.println("peerCount = " + peerCount);

                    //     });    
            } else {
                // server mode only - no peer to connect to
                network = factory.builder().buildAndStart();
                id = network.getNodeId();
                //Optional<String>  discAddr = network.getDiscoveryAddress();
                //Optional<String>  enr = network.getEnr();
                //System.out.println("id = " + id.toString());
                //System.out.println("discAddr = " + discAddr.toString());
                //System.out.println("enr = " + enr.get().substring(3));
                
            }
//            System.out.println("network node address: " + network.getNodeAddress());
            Optional<String>  discAddr = network.getDiscoveryAddress();
//            System.out.println("network discovery address: " + discAddr.get());
//            System.out.println("network node id: " + network.getNodeId());
            logger.warn("LOGGER nodeid: " + network.getNodeId() + " , nodeAddress: " + network.getNodeAddress() + " , discovery address: " + discAddr.get() );
            System.out.println("Starting discovery loop info");

    if(network != null) {
        Set<NodeRecord> activeKnownNodes = new HashSet<>();
        while (true) {
    //      List<NodeRecord> newActiveNodes =
              network
                  .streamPeers()
                  .filter(peer -> peer.getId() != null)
                  .forEach(
                      peer -> {
                          logger.debug("peer: id=" + peer.getId() + " address=" + peer.getAddress() + " isConnected=" + peer.isConnected());
                        }
                  );
                //   .map(NodeRecordInfo::getNode)
                //   .filter(r -> !activeKnownNodes.contains(r))
                //   .collect(Collectors.toList());
    
                network
                .streamKnownDiscoveryPeers()
                .filter(discoPeer -> discoPeer.getNodeAddress() != null)
                .forEach(
                    discoPeer -> {
                        //System.out.println("peer: id=" + peer.getId() + " address=" + peer.getAddress() + " isConnected=" + peer.isConnected());
                          logger.debug("discovery peer: " + discoPeer.getNodeAddress() + " pubkey="+discoPeer.getPublicKey());
                      }
                );
        //   activeKnownNodes.addAll(newActiveNodes);
        //   newActiveNodes.forEach(
        //       n -> {
        //         System.out.println(
        //             "New active node: "
        //                 + n.getNodeId()
        //                 + " @ "
        //                 + n.getUdpAddress().map(InetSocketAddress::toString).orElse("<unknown>"));
        //       });
          Thread.sleep(5000);
        }
        }
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }


    }

}

