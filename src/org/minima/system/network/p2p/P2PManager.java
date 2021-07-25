package org.minima.system.network.p2p;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.minima.Start;
import org.minima.utils.MinimaLogger;

import java.io.*;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.*;


/**
 * Design Notes
 * ------------
 * P2P Manager, controls what nodes we connect to.
 * To do this we need to keep the following information
 * - A list of all nodes we can connect to - Node(IP, LastSeen, connections, connectable, connected_to_this_node)
 * <p>
 * List is initialised from the save file or the hard coded list, if no save file is confirmed
 * We then need to check which of these nodes can be connected to
 * <p>
 * ----  func VerifyNodeList()
 * Iterates though list A doing CheckIsNodeAvailable each node on resp we update the entry in the list
 * ----  func HandshakeWithNode(Node) - Handshake (Request / Response is the same message)
 * <p>
 * At this point we have an up to date list of Nodes, all of which will be connectable
 * We don't want to shake hands with every node as that could be millions. So we cut it off at 100 responses or we have shaken hands with all known nodes, which ever comes first
 * <p>
 * ----  func List<Node> SelectNodesToConnect()
 * MAIN LOGIC - rough idea needs refinement!
 * Randomly select 10 nodes to connect to that, if possible, have < 10 peered connections each. If not possible, pick number so that there a set of NUM_HARDCODED_NODES possible nodes to connect to
 * <p>
 * Do this n times, so we have n sets of nodes, score each set of nodes by (total number of connected p2p nodes) - (total num leach connections) x LEACH_FACTOR
 * Connect to the most connected set.
 * LEACH_FACTOR arbitrarily selected, goal is to load balance the leachers, without needing to know if you are a leacher or not.
 * <p>
 * This should naturally ensure the most connected node set, whilst also keeping the connections fairly well distributed at a soft max of 10 connections per node
 * <p>
 * --- func List<Node> ConnectToPeers()
 * standard network connection stuff
 * <p>
 * <p>
 * --- func void DropUnHealthyNodes()
 * Remove node from list of known nodes, update p2p connections
 * <p>
 * On Losing a Node
 * --- func Node DrawNewNode()
 * Randomly selects a node from the available node pool
 * <p>
 * --- func bool IsNetworkHealthy()
 * Do something to check that we haven't degraded our connectivity to - total p2p network connections should be greater than this nodes connections and network score should be above 0
 * If false - try drop all connections and SelectNodesToConnect() again
 * <p>
 * --- func void SendHeartbeat()
 * Just send a heartbeat message to all nodes you are connected to
 * Every n minutes, maybe 10 min some arbitrary amount of time that wont swamp the network. Can be calculated eventually to account for less than 1% of network traffic
 * <p>
 * --- func void OnHeartbeat()
 * Updated node information and last seen time
 * <p>
 * <p>
 * Message P2PHandshake
 * MY_IP_V4 // Redundant Should be in the packet already
 * MY_IP_V6 // Redundant Should be in the packet already
 * Port
 * MY_NUM_P2P_CONNECTIONS
 * MY_NUM_LEACH_CONNECTIONS
 * MY_KNOWN_P2P_NODES
 * <p>
 * Message P2PHeartbeat
 * MY_IP_V4 // Redundant Should be in the packet already
 * Port
 * MY_NUM_P2P_CONNECTIONS
 * MY_NUM_LEACH_CONNECTIONS
 *
 * ####################################################################
 *
 * The P2PManager controls what nodes the minima node connects too.
 * It's a standalone class that isn't able to call the network functionality directly.
 * The P2PMessageProcessor has an instance of this class and calls the functions on this class to
 * generate the network messages that it needs to send and process the messages that have been received
 *
 * Written with the assumptions:
 *      - there will be a large number of nodes so we can't map the entire network
 *      - we don't know if we are a P2P node or a client node.
 *              We could be seen as both depending on the connectivity of the nodes we connect too
 */
public class P2PManager {

    private final InetAddress myIP;
    // activeNodeList.sort(Comparator.comparing(P2PNode::getLastSeenTime));
    private final int basePort;
    private final File p2pDataFile;
    private final HashMap<InetAddress, P2PNode> nodeMAP = new HashMap<>();
    // TODO: Ensure only 1 instance of a node is generated in memory
    // TODO: Need a map of Node by IP for easy lookup
    // TODO: Need a Sorted List of sorted by last seen time, which can then be used to prune dead nodes
    // TODO: Limit to 1000 activeNodes
    private ArrayList<P2PNode> activeNodeList = new ArrayList<>();
    private int numActiveP2PConnections;
    private int numClientConnections;


    /**
     * The default constructor for the P2P Manager
     * It loads the saved or hardcoded node list and sets them as the activeNodeList
     */
    public P2PManager(InetAddress myIP, int basePort, File p2pDataFile) {
        this.p2pDataFile = p2pDataFile;
        this.myIP = myIP;
        this.basePort = basePort;
        LoadNodeList();
    }

    /**
     * This constructor allows you to skip the node list loading.
     * It's intended purpose is for testing the P2PManager and should
     * not be used in the main loop
     *
     * @param nodeList The Node list to initialise the P2PManager with
     */
    public P2PManager(ArrayList<P2PNode> nodeList, InetAddress myIP, int basePort, File p2pDataFile) {
        this.activeNodeList = nodeList;
        this.p2pDataFile = p2pDataFile;
        this.myIP = myIP;
        this.basePort = basePort;
    }


    // Saving and Loading Node Lists

    public static Set<Integer> GetUniqueRandomNumbers(int max, int quantity) {
        Set<Integer> idxSet = new HashSet<>();
        Random rand = new Random();
        while (idxSet.size() < quantity) {
            idxSet.add(rand.nextInt(max));
        }
        return idxSet;
    }

    /**
     * Loads the node list from the saved node list file
     * If no node list file is present, it uses the hardcoded
     * official minima nodelist
     */
    public void LoadNodeList() {
        // Try and load node list from the saved data
        if (p2pDataFile.exists()) {
            try {
                FileInputStream inputStream = new FileInputStream(p2pDataFile);
                final ObjectMapper mapper = new ObjectMapper();
                this.activeNodeList = mapper.readValue(inputStream, new TypeReference<ArrayList<P2PNode>>() {
                });
            } catch (IOException ioe) {
                MinimaLogger.log("Error whilst reading in p2pDataFile: " + ioe);
            }
        } else {
            // If no data to load, then load the default list
            for (String ip : Start.VALID_BOOTSTRAP_NODES) {
                try {
                    InetAddress ipAddress = InetAddress.getByName(ip);
                    // We use -1 to represent not knowing the number of connections data or last seen time
                    P2PNode node = new P2PNode(ipAddress, 9001, -1, -1, -1, false, true);
                    activeNodeList.add(node);
                    nodeMAP.put(ipAddress, node);
                } catch (UnknownHostException exception) {
                    MinimaLogger.log("Node " + ip + " is not a valid INetAddress: " + exception);

                }
            }
        }
        // TODO: move this to only execute if db is loaded from json (unless I can determine if the load is ordered as its saved then I can just get rid of this)
        // Sort the list by lastSeenTimestamp
        activeNodeList.sort(Comparator.comparing(P2PNode::getLastSeenTimestamp));
    }

    // P2P Network Processors

    /**
     * Saves the activeNodeList to file as a json blob
     */
    public void SaveNodeList() {
        // Using jackson instead of your custom json system, sorry...
        final ObjectMapper mapper = new ObjectMapper();
        try {
            FileOutputStream fos = new FileOutputStream(p2pDataFile);
            final ByteArrayOutputStream out = new ByteArrayOutputStream();

            mapper.writeValue(out, activeNodeList);
            out.writeTo(fos);
            fos.close();
        } catch (IOException ioe) {
            MinimaLogger.log("Failed to write data to file: " + ioe);
        }
    }

    /**
     * Takes im a list of nodes and generates a handshake to send
     *
     * @param nodeList list of nodes to send handshakes request too
     * @return a list P2PHandshakeRequest to be sent out for handshake data
     */
    public ArrayList<P2PHandshake> GenHandshakeWithNodeList(ArrayList<P2PNode> nodeList) {
        ArrayList<P2PHandshake> handshakeMsgs = new ArrayList<P2PHandshake>();
        for (P2PNode node : nodeList) {
            P2PHandshake handshakeMsg = GenHandshakeForNode(node);
        }

        return handshakeMsgs;
    }

    /**
     * Creates a handshake request from this nodes data and the target nodes ip/port
     *
     * @param targetNode The targetNode to create the handshake request for
     * @return A request object that has all the information needed to send to the target targetNode
     */
    public P2PHandshake GenHandshakeForNode(P2PNode targetNode) {
        return new P2PHandshake(targetNode.getIPAddress(), targetNode.getPort(), myIP, basePort, numActiveP2PConnections, numClientConnections, activeNodeList);
    }

    public void ProcessHandshake(InetAddress ipaddress, int port, int numP2PConnections, int numClientConnections, ArrayList<P2PNode> receivedNodeList, boolean is_response) {
        long timestamp = System.currentTimeMillis();
        P2PNode node = null;
        if (nodeMAP.get(ipaddress) == null) {

            // if it's a response message we know its connectable
            node = new P2PNode(ipaddress, port, timestamp, numP2PConnections, numClientConnections, false, is_response);
            activeNodeList.add(node);
            nodeMAP.put(ipaddress, node);
        } else {
            node = nodeMAP.get(ipaddress);
            node.setLastSeenTime(timestamp);
            node.setNumActiveClientConnections(numClientConnections);
            node.setNumActiveP2PConnections(numP2PConnections);
            // We only want to override the connectable parameter if it's a response
            // There is no new information in a request
            if (is_response) {
                node.setConnectable(is_response);
            }
        }

        // Update the active node list if there is less than 1000 active nodes
        if (activeNodeList.size() < 1000) {
            for (P2PNode receivedNode : receivedNodeList) {
                // TODO: I don't like adding nodes we haven't confirmed to the activeNodeList. I need an unconfirmedNodesList
                if (nodeMAP.get(receivedNode.getIPAddress()) != null) {
                    node = new P2PNode(
                            receivedNode.getIPAddress(),
                            receivedNode.getPort(),
                            -1,
                            receivedNode.getNumActiveP2PConnections(),
                            receivedNode.getNumActiveClientConnections(),
                            false,
                            false);
                    activeNodeList.add(node);
                    nodeMAP.put(ipaddress, node);
                }
            }
        }
        SaveNodeList();
    }

    /**
     * When we receive a handshake request return a handshake response and
     * add the node data for node that sent the handshake request
     *
     * @param handshake the handshake that was received
     */
    public P2PHandshake OnReceiveHandshakeRequest(P2PHandshake handshake) {
        ProcessHandshake(
                handshake.getIPAddress(),
                handshake.getPort(),
                handshake.getNumP2PConnections(),
                handshake.getNumClientConnections(),
                handshake.getKnownNodesList(),
                false);
        return new P2PHandshake(
                handshake.getIPAddress(),
                handshake.getPort(),
                myIP,
                basePort,
                numActiveP2PConnections,
                numClientConnections,
                activeNodeList
        );
    }

    /**
     * When we receive a handshake response we update its data and we now know that it is connectable
     *
     * @param handshake the handshake that was received
     */
    public void OnReceiveHandshakeResponse(P2PHandshake handshake) {
        ProcessHandshake(
                handshake.getIPAddress(),
                handshake.getPort(),
                handshake.getNumP2PConnections(),
                handshake.getNumClientConnections(),
                handshake.getKnownNodesList(),
                true);
    }

    /**
     * Creates a P2PHeartbeat for the target node
     *
     * @param targetNode The node the heartbeat is to sent too
     * @return A P2PHeartbeat object
     */
    public P2PHeartbeat GenHeartbeatForNode(P2PNode targetNode) {
        return new P2PHeartbeat(targetNode.getIPAddress(), targetNode.getPort(), myIP, basePort, numActiveP2PConnections, numClientConnections);
    }

    /**
     * When we get a heatbeat we need to update the lastSeenTime for that node and the connection info
     *
     * @param heartbeat the heartbeat we received
     */
    public void OnReceiveHeartbeat(P2PHeartbeat heartbeat) {
        long timestamp = System.currentTimeMillis();
        P2PNode node = nodeMAP.get(heartbeat.getIPAddress());
        if (node != null) {
            node.setLastSeenTime(timestamp);
            node.setNumActiveP2PConnections(heartbeat.getNumP2PConnections());
            node.setNumActiveClientConnections(heartbeat.getNumClientConnections());
        } else {
            MinimaLogger.log("[!] Heartbeat received from unknown host");
        }

    }

    /**
     * When a connection has been established with a node we need to update the is_connected parameter
     * This connection could be from a client connecting to this node or from this node connecting to a p2p node
     *
     * @param nodesIp Ip of the node we have just connected too
     */
    public void OnConnectionEstablishedWithNode(InetAddress nodesIp) {
        P2PNode node = nodeMAP.get(nodesIp);
        if (node != null) {
            node.setConnectedToNode(true);
            // Note we don't update the nodes active connection data as we
            // don't know if this node is a p2p node or a client node
            if (node.isConnectable()) {
                numActiveP2PConnections += 1;
            } else {
                numClientConnections += 1;
            }
        }
    }

    // Managing P2P Node List

    /**
     * Called when a connection is safely shutdown or when we have determined a connection is dead
     * Update connection stats and remove the node from active nodes list
     *
     * @param nodesIp
     */
    public void OnDisconnectedFromNode(InetAddress nodesIp) {
        P2PNode node = nodeMAP.get(nodesIp);
        if (node != null) {
            node.setConnectedToNode(false);
            // Note we don't update the nodes active connection data as we
            // don't know if this node is a p2p node or a client node
            if (node.isConnectable()) {
                numActiveP2PConnections -= 1;
            } else {
                numClientConnections -= 1;
            }
        }
    }

    /**
     * Main Function!
     * Randomly select 10 nodes to connect to that, if possible, have < 10 peered connections each. If not possible, pick number so that there a set of NUM_HARDCODED_NODES possible nodes to connect to
     *
     * @return
     */
    public ArrayList<P2PNode> SelectNodesToConnect() {
        // What to do...
        // 1) Filter activeNodeList down to only nodes with < 10 and are connectable
        //    a) Whilst filtering bucket nodes by num p2p connections. We just need to track count per bucket
        //    b) If we can't meet the requirement of 10 nodes with < 10 connections. Use the bucketing to
        //       pick min number of connection that gets at least 10 nodes
        // 2) If more than 10 possible nodes create ii random sets of nodes
        // 3) Score each set - (total number of connected p2p nodes) - (total num client connections) x LEACH_FACTOR
        // 4) return the set with highest score
        int maxIdx = activeNodeList.size() - 1;
        //        ArrayList<ArrayList<P2PNode>> possibleCombinations;
        return null;
    }

    public void RemoveUnresponsiveNodes() {

    }

    public P2PNode DrawNewNode() {
        return null;
    }

    public boolean IsNetworkHealthy() {
        return false;
    }

    public ArrayList<P2PNode> GetActiveNodeList() {
        return activeNodeList;
    }

}
