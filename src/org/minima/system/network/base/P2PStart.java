package org.minima.system.network.base;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.Vector;
import java.util.concurrent.ExecutionException;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static io.netty.buffer.Unpooled.*;

// Import log4j classes.
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;
import org.minima.system.network.NetworkHandler;
import org.minima.system.network.base.DiscoveryNetworkFactory.DiscoveryNetworkBuilder;
import org.minima.system.network.base.LibP2PNetwork.PrivateKeyProvider;
import org.minima.system.network.base.libp2p.PrivateKeyGenerator;
//import org.minima.system.network.base.NodeRecordConverter;
import org.minima.system.network.base.peer.DiscoveryPeer;
import org.minima.system.network.base.peer.LibP2PNodeId;
import org.minima.system.network.base.peer.NodeId;
import org.minima.system.network.base.peer.Peer;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;

//import org.ethereum.beacon.discovery.schema.NodeRecord;
import org.ethereum.beacon.discovery.schema.NodeRecord;
import org.ethereum.beacon.discovery.schema.NodeRecordFactory;

import io.libp2p.core.PeerId;
import io.libp2p.core.crypto.PrivKey;
import io.libp2p.core.crypto.PubKey;
import io.libp2p.core.multiformats.Multihash;
import io.libp2p.crypto.keys.Secp256k1Kt;
import io.libp2p.etc.encode.Base58;
import io.netty.buffer.ByteBuf;
import io.libp2p.core.crypto.KEY_TYPE;
import io.libp2p.core.crypto.KeyKt;
//import io.libp2p.core.crypto.marshalPublicKey;

public class P2PStart extends MessageProcessor {

    private static final Logger logger = LogManager.getLogger(P2PStart.class);
    public static final String P2P_THREAD = "P2P";
    // these messages only control LIBP2P+DISCV5 <> Minima comms at the moment
    public static final String P2P_START_SCAN = "P2P_START_SCAN";
    public static final String P2P_STOP_SCAN = "P2P_STOP_SCAN";
    public static final String P2P_SAVE_NEIGHBOURS = "P2P_SAVE_NEIGHBOURS";
    public static final int    SAVE_NEIGHBOURS_DELAY = 10*1000; // save list every 10 seconds
    public static final String COMMA_DELIMITER = ",";
    private String mConfFolder;
    private NetworkHandler mNetwork;
    private DiscoveryNetwork<Peer> network;
    Set<InetSocketAddress> activeKnownNodes;
    Set<MinimaNodeInfo> allDiscoveredNodes = new HashSet<>();
    private NodeId nodeId;
    private PubKey pubKey;
    private File mP2PBootnodesFile;
    
    class MinimaNodeInfo {
        final public InetSocketAddress socket;
        final public String nodeRecord;
        final public String nodeID;
        final public String discoMultiAddrTCP;
        public MinimaNodeInfo(String nodeID, String nodeRecord, InetSocketAddress socket, String discoMultiAddrTCP) {
            this.nodeID = nodeID;
            this.nodeRecord = nodeRecord;
            this.socket = socket;
            this.discoMultiAddrTCP = discoMultiAddrTCP;
        }
    }

    // staticPeers = list of static peers in multiaddr format: /ip4/127.0.0.1/tcp/10219/p2p/16Uiu2HAmCnuHVjxoQtZzqenqjRr6hAja1XWCuC1SiqcWcWcp4iSt
    // bootnodes = list of ENR: enr:-Iu4QGvbP4hn3cxao3aFyZfeGBG0Ygp-KPJsK9h7pM_0FfCGauk0P2haW7AEiLaMLEDxRngy4SjCx6GGfwlsRBf0BBwBgmlkgnY0gmlwhH8AAAGJc2VjcDI1NmsxoQMCButDl63KBqEEyxV2R3nCvnHb7sEIgOACbb6yt6oxqYN0Y3CCJ-uDdWRwgifr 
    // ENR above in enr-viewer.com leads to: pubkey=0x030206eb4397adca06a104cb15764779c2be71dbeec10880e0026dbeb2b7aa31a9
    //  ip=127.0.0.1, tcp=10219, udp=10219
    // if we protobuf the pubkey (crypto.proto) we get 08021221030206eb4397adca06a104cb15764779c2be71dbeec10880e0026dbeb2b7aa31a9
    // 08 means varint follows with value 02. which means SECP256K1 key
    // 12 means String follows, starting with length. length is 0x21 or 33 dec
    // now we multihash it. Because the key is small (<50) it is inlined: 002508021221030206eb4397adca06a104cb15764779c2be71dbeec10880e0026dbeb2b7aa31a9
    // 00 -> Identity, 0x25=length (protobuf pubkey SECP256K1, 0x21+0x04)
    // and now we base58 encode it: 16Uiu2HAmCnuHVjxoQtZzqenqjRr6hAja1XWCuC1SiqcWcWcp4iSt
    // So the hex prefix to the pubkey to build the peerid is always 002508021221 (in our use case).
    public P2PStart(String zConfFolder, NetworkHandler minimaNet, String[] _staticPeers, String[] _bootnodes) {
        super(P2P_THREAD);
        this.mConfFolder = zConfFolder;
        mNetwork = minimaNet;
        Vector<String> staticPeers = new Vector<>();
        Vector<String> bootnodes   = new Vector<>();
        if(_staticPeers == null || _staticPeers.length == 0) {
            logger.info("P2P layer - no static peer provided by minima.");
        }
        if(_bootnodes == null || _bootnodes.length == 0) {
            logger.info("P2P layer - no bootnode provided by minima.");
        }
        if(_staticPeers != null && _bootnodes != null && _staticPeers.length == _bootnodes.length) {
            logger.info("P2P layer - received bootnode and staticpeer info from minima, adding to config");
            Collections.addAll(staticPeers, _staticPeers);
            Collections.addAll(bootnodes, _bootnodes);
        }

        // check config file for SECP256K1 private key
        File mRoot      = ensureFolder(new File(mConfFolder));
        String mRootPath  = mRoot.getAbsolutePath();
        
        //Current used TxPOW
        File mP2PDir   = ensureFolder(new File(mRoot,"p2p"));
        File mP2PNodePrivKeyFile = new File(mP2PDir, "NodePrivKey.pkey");
        mP2PBootnodesFile = new File(mP2PDir, "bootnodes.csv");

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
                PubKey pubKey = privKey.publicKey();
                System.out.println("Loaded private key from local dir: " + hex(privKey.bytes()));
                System.out.println("Computed public key: " + hex(pubKey.bytes()));
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
                System.out.println("Generated new private key and saved to local dir: " + hex(privKey.bytes()));
                System.out.println("Computed public key: " + hex(pubKey.bytes()) );
                outputStream.close();
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
        PeerId pid = PeerId.fromPubKey(privKey.publicKey());
        byte[] marshaledPubKey = KeyKt.marshalPublicKey(privKey.publicKey());
        nodeId = new LibP2PNodeId(pid);
        // if pubkey is less than 42 bytes then peerid will be 
        // the multihash digest of identity (0) and the pubkey as a protobuf (keytype, data)
        System.out.println("P2P layer - peerid - raw hex: " + pid.toHex()); 
        System.out.println("P2P layer - pubkey - raw hex: " + hex(privKey.publicKey().bytes()));
        System.out.println("P2P layer - protobuf pubkey: "  + P2PStart.hex(marshaledPubKey));
        System.out.println("P2P layer - protobuf pubkey length (dec): "  + marshaledPubKey.length);
        System.out.println("P2P layer - protobuf pubkey length (hex)): "  + Integer.toHexString(marshaledPubKey.length));
        ByteBuf wrappedBuf = wrappedBuffer(marshaledPubKey);
        System.out.println("P2P layer - wrapped buff protobuf pubkey (hex)): "  +  hex(wrappedBuf.array())); // Integer.toHexString(marshaledPubKey.length));
        System.out.println("P2P layer - wrapped buff protobuf pubkey capacity (dec): " + wrappedBuf.capacity());
        
        // this line below does not work. it should just add 0x0025 in front of the protobuf (identity+protobuf pubkey length).
        Multihash mhash = Multihash.digest(new Multihash.Descriptor(Multihash.Digest.Identity, null), wrappedBuf, (marshaledPubKey.length)*8);
        //System.out.println("P2P layer - multihash Str value: " + mhash.toString());
        // last 8 bytes are zeros for some reason 
        // as we are fixed size we only copy the meaningful bytes before Base58 encoding.
        byte[] mhashbytes = mhash.getBytes().copy(0, 39).array();
        System.out.println("P2P layer - multihash hex bytes value: " + hex(mhashbytes));
        System.out.println("P2P layer - multihash base58 value: " +    Base58.INSTANCE.encode(mhashbytes));

        System.out.println("P2P layer - peerid - base58 encoded: " + pid.toHex()); 
        System.out.println("P2P layer - nodeid (peerid base58): " + nodeId.toString());
        System.out.println("P2P layer - nodeid base58: " + nodeId.toBase58());
        pubKey = privKey.publicKey();


        // bootnodes
        if(mP2PBootnodesFile.exists()) {
            // try loading bootnodes from CSV file
            List<List<String>> records = new ArrayList<>();
            try (BufferedReader br = new BufferedReader(new FileReader(mP2PBootnodesFile))) {
                String line;
                while ((line = br.readLine()) != null) {
                    if(line.length() > 0) {
                        String[] values = line.split(P2PStart.COMMA_DELIMITER);
                        logger.debug("P2PStart:csv load: read and CSV split one line: " + line);
                        if(values.length == 2) { 
                            // expect p2p multiaddr and ENR
                            // TODO: add fields validation
                            records.add(Arrays.asList(values));
                            System.out.println("Loaded bootnode from CSV: " + records.get(records.size()-1).toString());
                        } else {
                            logger.warn("Failed to parse line from CSV: " + line);    
                        }
                    } else {
                        logger.debug("P2PStart: skipping empty line in CSV");
                    }
                }
            } catch(FileNotFoundException e) {

            } catch(IOException e) {

            }

            for( List<String> record: records) {
                staticPeers.add(record.get(0));
                bootnodes.add(record.get(1));
            }
        }

        DiscoveryNetworkFactory factory = new DiscoveryNetworkFactory();
        try {
            if(staticPeers != null && staticPeers.size() > 0 && bootnodes != null && bootnodes.size() > 0) {
                System.out.println("Building p2p layer using provided params: staticpeer=" + staticPeers.get(0) + " and bootnode=" + bootnodes.get(0));
                //TODO: refactor below line to loop over staticPeers and bootnode data
                DiscoveryNetworkBuilder builder = factory.builder();
                builder = builder.setPrivKey(privKey);
                for(int i = 0; i < staticPeers.size(); i++) {
                    builder = builder.staticPeer(staticPeers.get(i)).bootnode(bootnodes.get(i));
                }
                network = builder.buildAndStart();
//  network = factory.builder().setPrivKey(privKey).staticPeer(staticPeers[0]).bootnode(bootnodes[0]).buildAndStart();
            } else if(staticPeers == null || staticPeers.size() == 0) {
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
            PostMessage(P2P_SAVE_NEIGHBOURS);
            // PostTimerMessage(new TimerMessage(SAVE_NEIGHBOURS_DELAY, P2P_SAVE_NEIGHBOURS));
        } else {
            // initialization failed - what do we do?
            logger.error("Failed to start P2P network.");
        }
    }


    public static String hex(byte[] bytes) {
        StringBuilder result = new StringBuilder();
        for (byte aByte : bytes) {
            result.append(String.format("%02x", aByte));
            // upper case
            // result.append(String.format("%02X", aByte));
        }
        return result.toString();
    }

    public String convertToCSV(MinimaNodeInfo i) {
        StringBuilder builder = new StringBuilder();
        builder.append(i.discoMultiAddrTCP);
        builder.append(",");
        builder.append(i.nodeRecord);
        return builder.toString();
    }

    // from https://www.baeldung.com/java-csv
    public String convertToCSV(String[] data) {
        return Stream.of(data)
          .map(this::escapeSpecialCharacters)
          .collect(Collectors.joining(","));
    }

    public String escapeSpecialCharacters(String data) {
        String escapedData = data.replaceAll("\\R", " ");
        if (data.contains(",") || data.contains("\"") || data.contains("'")) {
            data = data.replace("\"", "\"\"");
            escapedData = "\"" + data + "\"";
        }
        return escapedData;
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
            p2pAddNewNodes();
            // p2pSaveNeighbours(); // for now save neighbours list after each scan
        } else if(zMessage.isMessageType(P2P_SAVE_NEIGHBOURS)) {
          //  p2pSaveNeighbours();
        }   
    }

    private void p2pSaveNeighbours() {
        logger.debug("p2pSaveNeighbours: trying to save neighbours list");
        System.out.println("p2pSaveNeighbours: trying to save neighbours list");
        if(allDiscoveredNodes.size() == 0) {
            logger.debug("p2pSaveNeighbours: empty nodes list, nothing to save, exiting.");
            return;
        }
        try {
            //File csvOutputFile = new File(CSV_FILE_NAME);
            File csvOutputFile = mP2PBootnodesFile;
            FileWriter csvWriter = new FileWriter(csvOutputFile);
            try (PrintWriter pw = new PrintWriter(csvWriter, true)) {
                for(MinimaNodeInfo i: allDiscoveredNodes) {
                    logger.debug("saving node i: " + i.nodeRecord);
                    pw.println(convertToCSV(i));
                }
//              allDiscoveredNodes.stream()
// //            dataLines.stream()
//               .map(this::convertToCSV)
//               .forEach(pw::println);
            }
            csvWriter.flush();
            csvWriter.close();
            //csvOutputFile.
            //assertTrue(csvOutputFile.exists());
        } catch(IOException e) {
            logger.warn("Could not save neighbours list to file! msg=" + e.getMessage());
        }
        logger.debug("p2pSaveNeighbours: end");
        // try {
        //     Thread.sleep(SAVE_NEIGHBOURS_DELAY);
        //     PostMessage(P2P_SAVE_NEIGHBOURS);
        // } catch(Exception e) {

        // }
    }

    private void p2pAddNewNodes() {
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
                network.streamKnownDiscoveryPeers()
                        .forEach(discoPeer -> { // disc peer node address should be inetsocketaddr
                            PeerId peerid = new PeerId(discoPeer.getNodeID().toArray());
                            // nodeAddress: enr_ip:enr_port
                            // pubkey:enr_secp256k1
                            // nodeid: derived(enr_secp256k1)
                            logger.debug("discovery peer: " + discoPeer.getNodeAddress() + " pubkey=" + discoPeer.getPublicKey()
                                        + " peerid: " + peerid 
                                        + " nodeid:" + discoPeer.getNodeID().toHexString() + " enr: " + discoPeer.getNodeRecord()); 
                            //TODO: establish link between Bytes nodeID and libp2p nodeid / peerid
                            //TODO: verify values for nodeid and enr and filter existing nodes vs new based on nodeid      
                            //Optional<DiscoveryPeer> tmpdiscopeer = NodeRecordConverter.convertToDiscoveryPeer(discoPeer.getNodeRecord());

                            //nodeRecord.getNodeId()
                            newActiveNodes.add(discoPeer.getNodeAddress());

                            if(!knownNodeIDs.contains(discoPeer.getNodeID().toString())) {
                                logger.debug("FOUND NEW NODE: nodeid:" + discoPeer.getNodeID().toString() + " " + discoPeer.getNodeRecord().toString());
                                MinimaNodeInfo aNewNodeInfo = new MinimaNodeInfo(discoPeer.getNodeID().toHexString(),
                                     discoPeer.getNodeRecord().toString(), 
                                    discoPeer.getNodeAddress(),
                                    getDiscoMultiAddrTCPFromENR(discoPeer.getNodeRecord(), discoPeer.getPublicKey().toArray())
                                );
                                unconnectedNewNodes.add(aNewNodeInfo);
                                allDiscoveredNodes.add(aNewNodeInfo);
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
                        logger.warn("IGNORING empty nodeRecord or nodeID."); 
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

                // save list
                p2pSaveNeighbours(); 
                try {
                    Thread.sleep(5000);
                    PostMessage(P2P_START_SCAN);
                } catch(Exception e) {

                }
            }
        }
    }

    public String getDiscoMultiAddrTCPFromENR(String ENR, byte[] marshalledPubKey)  {
        NodeRecord nodeRecord = NodeRecordFactory.DEFAULT.fromEnr(ENR);
        // extract multiaddress from nodeRecord
        nodeRecord.getTcpAddress();
        //nodeRecord.
        StringBuilder strBuilder = new StringBuilder();
        strBuilder.append("/ip4/");
        strBuilder.append(nodeRecord.getTcpAddress().get().getAddress().getHostAddress());
        strBuilder.append("/tcp/");
        strBuilder.append(nodeRecord.getTcpAddress().get().getPort());
        strBuilder.append("/p2p/");
        // construct nodeid from public key
        //discoPeer.getPublicKey()
//                            new PubKey(KeyType.Secp256k1);
        //PubKey hostPubKey = PubKey.fromString(discoPeer.getPublicKey());
        //discoPeer.getPublicKey();
//                            PubKey pubKey = Secp256k1Kt.unmarshalSecp256k1PublicKey(08021221 + discoPeer.getPublicKey().toArray());

        PubKey pubKey = Secp256k1Kt.unmarshalSecp256k1PublicKey(marshalledPubKey);
        PeerId pid = PeerId.fromPubKey(pubKey);
        nodeId = new LibP2PNodeId(pid);
        logger.debug("PubKey: " + hex(pubKey.bytes()));
        logger.debug("pid: " + pid.toHex());
        logger.debug("Built nodeid of discovered peer: " + nodeId.toBase58());
        strBuilder.append(nodeId.toBase58());
        logger.debug("Discovery TCP address: " + strBuilder.toString());
        logger.debug("discover peer (enr fields): " + nodeRecord.toString());
        return strBuilder.toString();
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
