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
import java.util.Hashtable;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.Vector;
import java.util.concurrent.ExecutionException;
import java.util.function.BiConsumer;
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
    Hashtable<String, MinimaNodeInfo> allDiscoveredNodes2 = new Hashtable<>();
    private NodeId nodeId;
    private PubKey pubKey;
    private File mP2PBootnodesFile;
    private File mRoot;

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
        mRoot      = ensureFolder(new File(mConfFolder));
        String mRootPath  = mRoot.getAbsolutePath();
        
        //Current used TxPOW
        File mP2PDir   = ensureFolder(new File(mRoot,"p2p"));
        File mP2PNodePrivKeyFile = new File(mP2PDir, "NodePrivKey.pkey");
        mP2PBootnodesFile = new File(mP2PDir, "bootnodes.csv");

        // two lines below just to get rid of temporary not initialized issue
        PrivateKeyProvider provider = PrivateKeyGenerator::generate;
        PrivKey privKey = provider.get();
        privKey = loadNodePrivateKey(mP2PNodePrivKeyFile);
        if(privKey == null) {
            privKey = generateNodePrivateKey(mP2PNodePrivKeyFile);
        }

        if(privKey == null) {
            System.out.println("P2P Error - priv key uninitialized!");
            logger.error("P2P Error - priv key uninitialized!");
            return;
        } 
        
        PeerId pid = PeerId.fromPubKey(privKey.publicKey());
        nodeId = new LibP2PNodeId(pid);
        pubKey = privKey.publicKey();

        //byte[] marshaledPubKey = KeyKt.marshalPublicKey(privKey.publicKey());
        // if pubkey is less than 42 bytes then peerid will be 
        // the multihash digest of identity (0) and the pubkey as a protobuf (keytype, data)
        // logger.debug("P2P layer - peerid - raw hex: " + pid.toHex()); 
        // logger.debug("P2P layer - pubkey - raw hex: " + hex(privKey.publicKey().bytes()));
        // logger.debug("P2P layer - protobuf pubkey: "  + P2PStart.hex(marshaledPubKey));
        // logger.debug("P2P layer - protobuf pubkey length (dec): "  + marshaledPubKey.length);
        // logger.debug("P2P layer - protobuf pubkey length (hex)): "  + Integer.toHexString(marshaledPubKey.length));
        //ByteBuf wrappedBuf = wrappedBuffer(marshaledPubKey);
        // logger.debug("P2P layer - wrapped buff protobuf pubkey (hex)): "  +  hex(wrappedBuf.array())); // Integer.toHexString(marshaledPubKey.length));
        // logger.debug("P2P layer - wrapped buff protobuf pubkey capacity (dec): " + wrappedBuf.capacity());
        // The line below does not work. It should just add 0x0025 in front of the protobuf (identity+protobuf pubkey length).
        //Multihash mhash = Multihash.digest(new Multihash.Descriptor(Multihash.Digest.Identity, null), wrappedBuf, (marshaledPubKey.length)*8);
        //System.out.println("P2P layer - multihash Str value: " + mhash.toString());
        // last 8 bytes are zeros for some reason 
        // as we are fixed size we only copy the meaningful bytes before Base58 encoding.
        //byte[] mhashbytes = mhash.getBytes().copy(0, 39).array();
        //logger.debug("P2P layer - multihash hex bytes value: " + hex(mhashbytes));
        //logger.debug("P2P layer - multihash base58 value: " +    Base58.INSTANCE.encode(mhashbytes)); // same as nodeId.toBase58()
        //logger.debug("P2P layer - peerid - base58 encoded: " + pid.toHex()); 
        //logger.debug("P2P layer - nodeid (peerid base58): " + nodeId.toString()); // same as .toBase58()
        logger.debug("P2P layer - nodeid base58: " + nodeId.toBase58());

        // bootnodes CSV
        List<List<String>> records = loadP2PNeighbours(mP2PBootnodesFile);
        if (records != null) {
            for (List<String> record : records) {
                staticPeers.add(record.get(1));
                bootnodes.add(record.get(2));
            }
        }

        DiscoveryNetworkFactory factory = new DiscoveryNetworkFactory();
        try {
            if(staticPeers != null && staticPeers.size() > 0 && bootnodes != null && bootnodes.size() > 0) {
                System.out.println("Building p2p layer using provided params: staticpeer=" + staticPeers.get(0) + " and bootnode=" + bootnodes.get(0));
                DiscoveryNetworkBuilder builder = factory.builder();
                builder = builder.setPrivKey(privKey);
                for(int i = 0; i < staticPeers.size(); i++) {
                    builder = builder.staticPeer(staticPeers.get(i)).bootnode(bootnodes.get(i));
                }
                network = builder.buildAndStart(mNetwork.getBasePort() + 5); 
            } else if(staticPeers == null || staticPeers.size() == 0) {
                logger.info("P2P: starting in standalone mode");
                System.out.println("P2P: starting in standalone mode");
                network = factory.builder().setPrivKey(privKey).buildAndStart(mNetwork.getBasePort() + 5);
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
            PostMessage(P2P_START_SCAN); // could also be a TimerMessage
            PostMessage(P2P_SAVE_NEIGHBOURS); // for now we save neighbours at the end of each scan, this timer is not used
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

    public String convertToCSV(String pubKey, MinimaNodeInfo i) {
        StringBuilder builder = new StringBuilder();
        builder.append(pubKey);
        builder.append(",");
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
            // saveP2PNeighbours(); // for now save neighbours list after each scan
        } else if(zMessage.isMessageType(P2P_SAVE_NEIGHBOURS)) {
          //  saveP2PNeighbours();
        }   
    }

    private PrivKey loadNodePrivateKey(File mP2PNodePrivKeyFile) {
        if (!mP2PNodePrivKeyFile.exists()) {
            return null;
        }
        PrivKey privKey = null;
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
        return privKey;
    }

    private PrivKey generateNodePrivateKey(File mP2PNodePrivKeyFile) {
        // generate a SECP256K1 private key
        // PrivateKeyProvider keyProvider = PrivateKeyGenerator::generate;
        PrivKey privKey = KeyKt.generateKeyPair(KEY_TYPE.SECP256K1).component1();
        // privKey = keyProvider.get();
        // save priv key to file
        try {
            FileOutputStream outputStream = new FileOutputStream(mP2PNodePrivKeyFile);
            outputStream.write(KeyKt.marshalPrivateKey(privKey));
            //System.out.println("Generated new node private key and saved to local dir: " + hex(privKey.bytes()));
            System.out.println("Computed node public key: " + hex(pubKey.bytes()));
            outputStream.close();
        } catch (Exception e) {
            System.out.println("Failed to save node private key to disk - " + e.getMessage());
            logger.error("Failed to save node private key to disk - " + e.getMessage());
        }
        return privKey;
    }

    private List<List<String>> loadP2PNeighbours(File mP2PBootnodesFile) {
        if(!mP2PBootnodesFile.exists()) {
            return null;
        }
        
        // try loading bootnodes from CSV file
        List<List<String>> records = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(mP2PBootnodesFile))) {
            String line;
            logger.debug("starting to read p2p bootnodes CSV file: " + mP2PBootnodesFile.getAbsolutePath());
            while ((line = br.readLine()) != null) {
                if (line.length() > 0) {
                    String[] values = line.split(P2PStart.COMMA_DELIMITER);
                    logger.debug("P2PStart:csv load: read and CSV split one line: " + line);
                    if (values.length == 2) {
                        // expect public key, p2p multiaddr and ENR
                        // TODO: add fields validation
                        records.add(Arrays.asList(values));
                        //System.out.println("Loaded bootnode from CSV: " + records.get(records.size() - 1).toString());
                        logger.info("Loaded bootnode from CSV: " + records.get(records.size() - 1).toString());
                    } else {
                        logger.error("Failed to parse line from CSV: " + line);
                        System.out.println("P2P - error parsing line from CSV: " + line);
                    }
                } else {
                    logger.debug("P2PStart: skipping empty line in CSV");
                }
            }
        } catch (FileNotFoundException e) {

        } catch (IOException e) {

        }
        return records;
    }

    private void saveP2PNeighbours() {
        logger.debug("saveP2PNeighbours: trying to save neighbours list");
        if(allDiscoveredNodes2.size() == 0) {
            logger.debug("savep2pNeighbours: empty nodes list, nothing to save, exiting.");
            return;
        }
        try {
            //File csvOutputFile = new File(CSV_FILE_NAME);
            File csvOutputFile = mP2PBootnodesFile;
            FileWriter csvWriter = new FileWriter(csvOutputFile);
            try (PrintWriter pw = new PrintWriter(csvWriter, true)) { 
                allDiscoveredNodes2.forEach(new BiConsumer<String, MinimaNodeInfo>() {
                    @Override
                    public void accept(String pubKey, MinimaNodeInfo i) {
                        logger.debug("saving node i: pubkey=" +  pubKey + " enr=" + i.nodeRecord + " line=" + convertToCSV(pubKey, i));
                        pw.println(convertToCSV(pubKey, i));
                    }
                });
            } catch(Exception e) {
                logger.warn("Could not write lines to neighbours list file! " + "msg=" + e.getMessage());
                e.printStackTrace();
            }
            //csvWriter.flush();
        } catch(IOException e) {
            logger.warn("Could not flush and close neighbours list file! " + "msg=" + e.getMessage());
            e.printStackTrace();
        }
        logger.debug("saveP2PNeighbours: end");
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
                                allDiscoveredNodes2.put(aNewNodeInfo.nodeID, aNewNodeInfo);
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
                saveP2PNeighbours(); 
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
    }
            
    private static File ensureFolder(File zFolder) {
        if(!zFolder.exists()) {
                zFolder.mkdirs();
        }
        
        return zFolder;
    }

}
