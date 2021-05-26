package org.minima.system.network.base;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.stream.Stream;

// Import log4j classes.
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;
import org.minima.system.network.NetworkHandler;
import org.minima.system.network.base.LibP2PNetwork.PrivateKeyProvider;
import org.minima.system.network.base.libp2p.PrivateKeyGenerator;
import org.minima.system.network.base.peer.DiscoveryPeer;
import org.minima.system.network.base.peer.LibP2PNodeId;
import org.minima.system.network.base.peer.NodeId;
import org.minima.system.network.base.peer.Peer;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;

import io.libp2p.core.PeerId;
import io.libp2p.core.crypto.PrivKey;
import io.libp2p.core.crypto.PubKey;
import io.libp2p.core.crypto.KEY_TYPE;
import io.libp2p.core.crypto.KeyKt;

public class P2PStart extends MessageProcessor {

    private static final Logger logger = LogManager.getLogger(P2PStart.class);
    public static final String P2P_THREAD = "P2P";
    // these messages only control LIBP2P+DISCV5 <> Minima comms at the moment
    public static final String P2P_START_SCAN = "P2P_START_SCAN";
    public static final String P2P_STOP_SCAN = "P2P_STOP_SCAN";
    
    private String mConfFolder;
    private NetworkHandler mNetwork;
    private DiscoveryNetwork<Peer> network;
    Set<InetSocketAddress> activeKnownNodes;
    private NodeId nodeId;
    private PubKey pubKey;
    
    class MinimaNodeInfo {
        final public InetSocketAddress socket;
        final public String nodeRecord;
        final public String nodeID;
        public MinimaNodeInfo(String nodeID, String nodeRecord, InetSocketAddress socket) {
            this.nodeID = nodeID;
            this.nodeRecord = nodeRecord;
            this.socket = socket;
        }
    }

    // staticPeers = list of static peers in multiaddr format: /ip4/127.0.0.1/tcp/10219/p2p/16Uiu2HAmCnuHVjxoQtZzqenqjRr6hAja1XWCuC1SiqcWcWcp4iSt
    // bootnodes = list of ENR: enr:-Iu4QGvbP4hn3cxao3aFyZfeGBG0Ygp-KPJsK9h7pM_0FfCGauk0P2haW7AEiLaMLEDxRngy4SjCx6GGfwlsRBf0BBwBgmlkgnY0gmlwhH8AAAGJc2VjcDI1NmsxoQMCButDl63KBqEEyxV2R3nCvnHb7sEIgOACbb6yt6oxqYN0Y3CCJ-uDdWRwgifr 
    public P2PStart(String zConfFolder, NetworkHandler minimaNet, String[] staticPeers, String[] bootnodes) {
        super(P2P_THREAD);
        this.mConfFolder = zConfFolder;
        mNetwork = minimaNet;
        if(staticPeers == null || staticPeers.length == 0) {
            logger.info("P2P layer - no static peer.");
        }
        if(bootnodes == null || bootnodes.length == 0) {
            logger.info("P2P layer - no bootnode.");
        }


        // check config file for SECP256K1 private key
        File mRoot      = ensureFolder(new File(mConfFolder));
        String mRootPath  = mRoot.getAbsolutePath();
        
        //Current used TxPOW
        File mP2PDir   = ensureFolder(new File(mRoot,"p2p"));
        File mP2PNodePrivKeyFile = new File(mP2PDir, "NodePrivKey.pkey");
        // two lines below just to get rid of temporary not initialized issue
        PrivateKeyProvider provider = PrivateKeyGenerator::generate;
        PrivKey privKey = provider.get();
        if(mP2PNodePrivKeyFile.exists()) {
            // try loading file from private key
            // if error, bailout?
            FileInputStream inputStream;
            try {
                inputStream = new FileInputStream(mP2PNodePrivKeyFile);
                byte[] keyBuffer;
                keyBuffer = inputStream.readAllBytes();
                privKey = KeyKt.unmarshalPrivateKey(keyBuffer);
            } catch (FileNotFoundException e) {
                System.out.println("Failed to read private key from disk - " + e.getMessage());
                e.printStackTrace();
             } catch (IOException e) {
                System.out.println("Failed to read private key from disk - " + e.getMessage());
                e.printStackTrace();
            }
        } else {
            // generate a SECP256K1 private key
            //PrivateKeyProvider keyProvider = PrivateKeyGenerator::generate;
            privKey = KeyKt.generateKeyPair(KEY_TYPE.SECP256K1).component1();
            // privKey = keyProvider.get();
            // save priv key to file
            try {
                FileOutputStream outputStream = new FileOutputStream(mP2PNodePrivKeyFile);
                outputStream.write(KeyKt.marshalPrivateKey(privKey));
            } catch (Exception e) {
                System.out.println("Failed to save private key to disk - " + e.getMessage());
            }
        }
        if(privKey == null) {
            System.out.println("P2P Error - priv key uninitialized!");
            return;
        } 
        // privKey.toString();
        logger.warn("P2P layer - generated node private key: " + privKey.toString());
        System.out.println("P2P layer - generated node private key: " + privKey.toString());
        //System.out.println("P2P layer - generated node private key bytes: " + String. privKey.raw();
        // generate ENR nodeid - not to be confused with libp2pnodeid which is session specific!
        nodeId = new LibP2PNodeId(PeerId.fromPubKey(privKey.publicKey()));
        System.out.println("P2P layer - nodeid: " + nodeId.toString());
        System.out.println("P2P layer - nodeid base58: " + nodeId.toBase58());
        pubKey = privKey.publicKey();

        DiscoveryNetworkFactory factory = new DiscoveryNetworkFactory();
        try {
            if(staticPeers != null && staticPeers.length > 0 && bootnodes != null && bootnodes.length > 0) {
                System.out.println("Building p2p layer using provided params: staticpeer=" + staticPeers[0] + " and bootnode=" + bootnodes[0]);
                network = factory.builder().setPrivKey(privKey).staticPeer(staticPeers[0]).bootnode(bootnodes[0]).buildAndStart();
            } else if(staticPeers == null || staticPeers.length == 0) {
                logger.info("P2P: starting in standalone mode");
                System.out.println("P2P: starting in standalone mode");
                network = factory.builder().setPrivKey(privKey).buildAndStart();
            }
        } catch (Exception e) {
            logger.error("P2P failed to start through DiscoveyrNetworkFactory.");
            e.printStackTrace();
        }

        if(network != null) {
            // P2P initialization complete
            Optional<String> discAddr = network.getDiscoveryAddress();
            logger.warn("LOGGER nodeid: " + network.getNodeId() + " , nodeAddress: " + network.getNodeAddress()
                    + " , discovery address: " + discAddr.get());
            System.out.println("P2P nodeid: " + network.getNodeId() + " , nodeAddress: " + network.getNodeAddress()
            + " , discovery address: " + discAddr.get());
            System.out.println("Starting discovery loop info");
            activeKnownNodes = new HashSet<>();
            // TODO: send to yourself a message 5 seconds in the future
            PostMessage(P2P_START_SCAN); // could also be a TimerMessage
        } else {
            // initialization failed - what do we do?
            logger.error("Failed to start P2P network.");
        }
    }

    public NodeId getNodeId() {
        return nodeId;
    }

    public PubKey getPubKey() {
        return pubKey;
    }

    public String getENR() {
        return network.getENR();
    }

    public Optional<String> getDiscoveryAddress() {
        return network.getDiscoveryAddress();
    }
    
    public int getP2PPeerCount() {
       // return network.get
       return network.getP2PPeerCount();
    }

    public Stream<Peer> streamPeers() {
        return network.streamPeers();
    }

    public Stream<DiscoveryPeer> streamDiscoveryPeers() {
        return network.streamKnownDiscoveryPeers();
    }


    @Override
    protected void processMessage(Message zMessage) throws Exception {
        logger.warn("P2PStart received message: " + zMessage.toString());

        if(zMessage.isMessageType(P2P_START_SCAN)) {
       
          if(network != null) {
            Set<InetSocketAddress> activeKnownNodes = new HashSet<>();
            while (true) {

                // we dont really care about this list...
                network.streamPeers().filter(peer -> peer.getId() != null).forEach(peer -> {
                    logger.debug("peer: id=" + peer.getId()); // peer address == peer id and " isConnected=" true
                    
                });
                                
                ArrayList<MinimaClient> mClients = mNetwork.getNetClients();

                Set<String> knownNodeIDs = new HashSet<>();

                for(MinimaClient mClient: mClients) {
                    logger.debug(" mclient nodeid=" + mClient.getNodeID() + ", nodeRecord=" + mClient.getNodeRecord());
                    knownNodeIDs.add(mClient.getNodeID());
                }

                Set<InetSocketAddress> newActiveNodes = new HashSet<>();
                Set<MinimaNodeInfo> unconnectedNewNodes = new HashSet<>();
                //logger.debug("trying to stream discovery peers");
                network.streamKnownDiscoveryPeers()
                        .forEach(discoPeer -> { // disc peer node address should be inetsocketaddr
                            PeerId peerid = new PeerId(discoPeer.getNodeID().toArray());
                            logger.debug("discovery peer: " + discoPeer.getNodeAddress() + " pubkey=" + discoPeer.getPublicKey()
                                        + " peerid: " + peerid 
                                        + " nodeid:" + discoPeer.getNodeID().toHexString() + " enr: " + discoPeer.getNodeRecord()); 
                                        //TODO: establish link between Bytes nodeID and libp2p nodeid / peerid
                                        //TODO: verify values for nodeid and enr and filter existing nodes vs new based on nodeid      
                            newActiveNodes.add(discoPeer.getNodeAddress());

                            if(!knownNodeIDs.contains(discoPeer.getNodeID().toString())) {
                                logger.debug("FOUND NEW NODE: nodeid:" + discoPeer.getNodeID().toString() + " " + discoPeer.getNodeRecord().toString());
                                unconnectedNewNodes.add(new MinimaNodeInfo(discoPeer.getNodeID().toHexString(),
                                                                            discoPeer.getNodeRecord().toString(), 
                                                                    discoPeer.getNodeAddress()));
                            } else {
                                logger.debug("SKIPPING an already connected node: nodeid:" + discoPeer.getNodeID().toHexString() + " " + discoPeer.getNodeRecord().toString());
                            }
                        });

                
                Set<InetSocketAddress> delta = new HashSet<InetSocketAddress>(newActiveNodes);
                delta.removeAll(activeKnownNodes); //now contains only new sockets

                for(MinimaNodeInfo i: unconnectedNewNodes) {
                    logger.info("New peer address: " + i.socket.toString().substring(1));
                    // TODO: replace ENR with nodeID, but P2PStart.nodeID is not the correct value (16... and not the bytes)
                    if (i.nodeRecord.compareTo(network.getENR())==0) { 
                        logger.warn("IGNORING node ENR in list of new peers."); 
                    } else if(i.nodeRecord == null || i.nodeID == null) {
                        logger.warn("IGNORING empty ndoeRecord or nodeID."); 
                    } else {
                        logger.info("CONNECTING to new ENR " + i.nodeRecord);
                        System.out.println("Starting MinimaClient: " + i.socket.toString().substring(1) + ":9001");
                        String nodeRecord = i.nodeRecord, nodeID = i.nodeID;
                        MinimaClient mclient = new MinimaClient(i.socket.getAddress().toString().substring(1), 9001, mNetwork, nodeID, nodeRecord); // hardcode port for now
                        mNetwork.PostMessage(new Message(NetworkHandler.NETWORK_NEWCLIENT).addObject("client", mclient));
                    }
                }

                // update known nodes
                activeKnownNodes = newActiveNodes;

                try {
                    Thread.sleep(5000);
                    PostMessage(P2P_START_SCAN);
                } catch(Exception e) {

                }
            }
          }
        }

            
    }

    //TODO: refactor below code to use above object and constructor instead - if possible
    public static void main(String[] args) throws ExecutionException, InterruptedException {
        System.out.println("Hello world!");
        // attempt 1: start with DiscoveryNetworkFactory
        DiscoveryNetworkFactory factory = new DiscoveryNetworkFactory();
        final DiscoveryNetwork<Peer> network;
        try {

            String node1_id;
            String[] node1_addr_fields;

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
                                           // DiscV5 cant start without at least one boot ndoe
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
                //id = network.getNodeId();
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
                    //logger.debug("trying to stream discovery peers");
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

            
    private static File ensureFolder(File zFolder) {
        if(!zFolder.exists()) {
                zFolder.mkdirs();
        }
        
        return zFolder;
    }

}
