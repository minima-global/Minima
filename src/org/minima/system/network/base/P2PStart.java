package org.minima.system.network.base;

import java.net.InetSocketAddress;
import java.util.HashSet;
import java.util.Iterator;
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

    public static void main(String[] args) throws ExecutionException, InterruptedException {
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
                node1_id = node1_addr_fields[node1_addr_fields.length - 1];
                String bootnode_1 = args[1];
                network = factory.builder().staticPeer(args[0]).bootnode(bootnode_1).buildAndStart();
                LibP2PNodeId id_1 = new LibP2PNodeId(PeerId.fromBase58(node1_id));

            } else if (args.length == 1) { // first is p2p addr - deprecated - should start with p2p addr and ENR
                                           // (application level address)
                logger.warn("Careful! This mode is deprecated, either start with 0 or 2 args, not 1.");
                System.out.println("Found addr: " + args[0]);
                node1_addr_fields = args[0].split("/");
                node1_id = node1_addr_fields[node1_addr_fields.length - 1];

                System.out.println("Found node1_id: " + node1_id);
                // Multiaddr address = Multiaddr.fromString(args[0]);
                network = factory.builder().staticPeer(args[0]).buildAndStart();
            } else {
                // server mode only - no peer to connect to
                network = factory.builder().buildAndStart();
                id = network.getNodeId();
            }

            Optional<String> discAddr = network.getDiscoveryAddress();
            logger.warn("LOGGER nodeid: " + network.getNodeId() + " , nodeAddress: " + network.getNodeAddress()
                    + " , discovery address: " + discAddr.get());
            System.out.println("Starting discovery loop info");

            if (network != null) {
                Set<InetSocketAddress> activeKnownNodes = new HashSet<>();
                while (true) {
                    network.streamPeers().filter(peer -> peer.getId() != null).forEach(peer -> {
                        logger.debug("peer: id=" + peer.getId()); // peer address == peer id and " isConnected=" true
                    });

                    Set<InetSocketAddress> newActiveNodes = new HashSet<>();
                    //logger.debug("trying to stream dicovery peers");
                    network.streamKnownDiscoveryPeers()
                            .forEach(discoPeer -> { // disc peer node address should be inetsocketaddr
                              //  logger.debug("discovery peer: " + discoPeer.getNodeAddress() + " pubkey=" + discoPeer.getPublicKey());         
                                newActiveNodes.add(discoPeer.getNodeAddress());
                            });

                    Set<InetSocketAddress> delta = new HashSet<InetSocketAddress>(newActiveNodes);
                    delta.removeAll(activeKnownNodes); //now contains only new sockets
                           
                    for(InetSocketAddress i: delta) {
                        logger.info("New peer address: " + i.toString());
                    }

                    // update known nodes
                    activeKnownNodes = newActiveNodes;

                    Thread.sleep(5000);
                }
            }
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

    }

}
