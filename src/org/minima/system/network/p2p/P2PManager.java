package org.minima.system.network.p2p;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.Getter;
import org.minima.Start;
import org.minima.utils.MinimaLogger;

import java.io.*;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.UnknownHostException;
import java.util.*;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.TimeUnit;


/**
Design Notes
------------
P2P Manager, controls what nodes we connect to.
To do this we need to keep the following information
- A list of all nodes we can connect to - Node(IP, LastSeen, connections, connectable, connected_to_this_node)

List is initialised from the save file or the hard coded list, if no save file is confirmed
We then need to check which of these nodes can be connected to

----  func VerifyNodeList()
Iterates though list A doing CheckIsNodeAvailable each node on resp we update the entry in the list
----  func HandshakeWithNode(Node) - Handshake (Request / Response is the same message)

At this point we have an up to date list of Nodes, all of which will be connectable
We don't want to shake hands with every node as that could be millions. So we cut it off at 100 responses or we have shaken hands with all known nodes, which ever comes first

----  func List<Node> SelectNodesToConnect()
MAIN LOGIC - rough idea needs refinement!
Randomly select 10 nodes to connect to that, if possible, have < 10 peered connections each. If not possible, pick number so that there a set of NUM_HARDCODED_NODES possible nodes to connect to

Do this n times, so we have n sets of nodes, score each set of nodes by (total number of connected p2p nodes) - (total num leach connections) x LEACH_FACTOR
Connect to the most connected set.
LEACH_FACTOR arbitrarily selected, goal is to load balance the leachers, without needing to know if you are a leacher or not.

This should naturally ensure the most connected node set, whilst also keeping the connections fairly well distributed at a soft max of 10 connections per node

--- func List<Node> ConnectToPeers()
standard network connection stuff


--- func void DropUnHealthyNodes()
Remove node from list of known nodes, update p2p connections

On Losing a Node
--- func Node DrawNewNode()
Randomly selects a node from the available node pool

--- func bool IsNetworkHealthy()
Do something to check that we haven't degraded our connectivity to - total p2p network connections should be greater than this nodes connections and network score should be above 0
If false - try drop all connections and SelectNodesToConnect() again

--- func void SendHeartbeat()
Just send a heartbeat message to all nodes you are connected to
Every n minutes, maybe 10 min some arbitrary amount of time that wont swamp the network. Can be calculated eventually to account for less than 1% of network traffic

--- func void OnHeartbeat()
Updated node information and last seen time


Message P2PHandshake
MY_IP_V4 // Redundant Should be in the packet already
MY_IP_V6 // Redundant Should be in the packet already
Port
MY_NUM_P2P_CONNECTIONS
MY_NUM_LEACH_CONNECTIONS
MY_KNOWN_P2P_NODES

Message P2PHeartbeat
MY_IP_V4 // Redundant Should be in the packet already
Port
MY_NUM_P2P_CONNECTIONS
MY_NUM_LEACH_CONNECTIONS
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
@Getter
public class P2PManager {

    private final static int NUMBER_NODES_TO_CONNECT_TO = 10;
    private final static int KNOWN_NODES_SOFT_LIMIT = 1000;
    private final static int KNOWN_NODES_FUDGE_FACTOR = 200;
    private final static int NUMBER_OF_NODES_TO_SHARE = 200;
    private final static int TIME_UNTIL_NODE_REMOVED_MINS = 30;

    private final P2PNode node;
    private final File p2pDataFile;
    // TODO: Need a Sorted List of sorted by last seen time, which can then be used to prune dead nodes
    private ArrayList<P2PNode> verifiedP2PNodeList = new ArrayList<>();
    private HashMap<InetSocketAddress, P2PNode> verifiedP2PNodeMap = new HashMap<>();
    private ArrayList<P2PNode> unverifiedP2PNodeList = new ArrayList<>();
    private HashMap<InetSocketAddress, P2PNode> unverifiedP2PNodeMap = new HashMap<>();

    // activeNodeList.sort(Comparator.comparing(P2PNode::getLastSeenTime));



    /**
     * The default constructor for the P2P Manager
     * It loads the saved or hardcoded node list and sets them as the activeNodeList
     */
    public P2PManager(InetSocketAddress myIP, File p2pDataFile) {
        this.p2pDataFile = p2pDataFile;
        this.node = new P2PNode(myIP, 0, null, null, true, true);
        LoadNodeList();
    }

    /**
     * This constructor allows you to skip the node list loading.
     * It's intended purpose is for testing the P2PManager and should
     * not be used in the main loop
     *
     * @param nodeList The Node list to initialise the P2PManager with
     */
    public P2PManager(ArrayList<P2PNode> nodeList, InetSocketAddress myIP, File p2pDataFile) {
        this.unverifiedP2PNodeList = nodeList;
        for (P2PNode node: this.unverifiedP2PNodeList){
            this.unverifiedP2PNodeMap.put(node.getIPAddress(), node);
        }
        this.p2pDataFile = p2pDataFile;
        this.node = new P2PNode(myIP, 0, null, null, true, true);

    }


    // Saving and Loading Node Lists
    /**
     * Loads the node list from the saved node list file
     * If no node list file is present, it uses the hardcoded
     * official minima nodelist
     */
    public void LoadNodeList() {
        // Try and load node list from the saved data
        this.unverifiedP2PNodeList = new ArrayList<>();
        this.unverifiedP2PNodeMap = new HashMap<>();
        this.verifiedP2PNodeList = new ArrayList<>();
        this.verifiedP2PNodeMap = new HashMap<>();
        if (p2pDataFile.exists()) {
            try {
                FileInputStream inputStream = new FileInputStream(p2pDataFile);
                final ObjectMapper mapper = new ObjectMapper();
                this.unverifiedP2PNodeList = mapper.readValue(inputStream, new TypeReference<ArrayList<P2PNode>>() {});
                for (P2PNode node: this.unverifiedP2PNodeList){
                    this.unverifiedP2PNodeMap.put(node.getIPAddress(), node);
                }
            } catch (IOException ioe) {
                MinimaLogger.log("Error whilst reading in p2pDataFile: " + ioe);
            }
        } else {
            // If no data to load, then load the default list
            for (String ip : Start.VALID_BOOTSTRAP_NODES) {
                try {
                    InetSocketAddress ipAddress = new InetSocketAddress(InetAddress.getByName(ip), 9001);
                    // We use -1 to represent not knowing the number of connections data or last seen time
                    P2PNode node = new P2PNode(ipAddress, -1, null, null, false, true);
                    unverifiedP2PNodeList.add(node);
                    unverifiedP2PNodeMap.put(ipAddress, node);
                } catch (UnknownHostException exception) {
                    MinimaLogger.log("Node " + ip + " is not a valid InetSocketAddress: " + exception);

                }
            }
        }
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

            mapper.writeValue(out, verifiedP2PNodeList);
            out.writeTo(fos);
            fos.close();
        } catch (IOException ioe) {
            MinimaLogger.log("Failed to write data to file: " + ioe);
        }
    }

    /**
     * Takes im a list of nodes and generates a handshake to send
     *
     * @return a list P2PHandshakeRequest to be sent out for handshake data
     */
    public ArrayList<P2PHandshake> GenHandshakeWithUnverifiedNodes() {
        ArrayList<P2PHandshake> handshakeMsgs = new ArrayList<>();
        for (P2PNode node : this.unverifiedP2PNodeList) {
            handshakeMsgs.add(GenHandshakeForNode(node));
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
        return new P2PHandshake(targetNode, this.node, this.GetVerifiedNodeListSubset());
    }

    public ArrayList<P2PNode> GetVerifiedNodeListSubset(){
        ArrayList<P2PNode> out = new ArrayList<>(NUMBER_OF_NODES_TO_SHARE);
        Iterator<P2PNode> iterator = verifiedP2PNodeList.iterator();
        for (int i = 0; i < NUMBER_OF_NODES_TO_SHARE && iterator.hasNext(); i++) {
            out.add(iterator.next());
        }
        return out;
    }

    public void UpdateNodeLists(P2PNode receivedNode){
        long timestamp = System.currentTimeMillis();
        receivedNode.setLastSeenTimestamp(timestamp);
        if ((verifiedP2PNodeMap.get(receivedNode.getIPAddress()) == null) && (unverifiedP2PNodeMap.get(receivedNode.getIPAddress()) != null)){
            // If we had this node in the unverified list already then move it to verified
            P2PNode newlyVerifiedNode = unverifiedP2PNodeMap.remove(receivedNode.getIPAddress());
            newlyVerifiedNode.setLastSeenTimestamp(timestamp);
            newlyVerifiedNode.setConnectedP2PNodes(receivedNode.getConnectedP2PNodes());
            newlyVerifiedNode.setConnectedClientNodes(receivedNode.getConnectedClientNodes());
            unverifiedP2PNodeList.remove(newlyVerifiedNode);
            verifiedP2PNodeList.add(newlyVerifiedNode);
            verifiedP2PNodeMap.put(newlyVerifiedNode.getIPAddress(), newlyVerifiedNode);
        } else if ((verifiedP2PNodeMap.get(receivedNode.getIPAddress()) == null) && (unverifiedP2PNodeMap.get(receivedNode.getIPAddress()) == null)){
            // This is a totally new node which we can add to the unverifiedP2PNodeList
            unverifiedP2PNodeList.add(receivedNode);
            unverifiedP2PNodeMap.put(receivedNode.getIPAddress(), receivedNode);
        } else {
            // This is a verified node
            P2PNode verifiedNode = verifiedP2PNodeMap.get(receivedNode.getIPAddress());
            verifiedNode.setConnectedP2PNodes(receivedNode.getConnectedP2PNodes());
            verifiedNode.setConnectedClientNodes(receivedNode.getConnectedClientNodes());
            verifiedNode.setLastSeenTimestamp(timestamp);
        }
        verifiedP2PNodeList.sort(Comparator.comparing(P2PNode::getLastSeenTimestamp));
        unverifiedP2PNodeList.sort(Comparator.comparing(P2PNode::getLastSeenTimestamp));
    }

    public void ProcessHandshake(P2PNode receivedNode, ArrayList<P2PNode> receivedNodeList, boolean is_response) {
        if (is_response){
            UpdateNodeLists(receivedNode);
            P2PNode nodeToUpdate = verifiedP2PNodeMap.get(receivedNode.getIPAddress());
            if (nodeToUpdate != null) {
                nodeToUpdate.setConnectable(true);
            } else {
                MinimaLogger.log("Got a response message from a node that's not being tracked!");
            }
        } else{
            UpdateNodeLists(receivedNode);
        }

        if (receivedNodeList != null && !receivedNodeList.isEmpty()) {
            // Update the active node list if there is less than 1200 active nodes
            if ((verifiedP2PNodeList.size() + unverifiedP2PNodeList.size()) < KNOWN_NODES_SOFT_LIMIT + KNOWN_NODES_FUDGE_FACTOR) {
                for (P2PNode nodeFromHandshake : receivedNodeList) {
                    if ((verifiedP2PNodeMap.get(receivedNode.getIPAddress()) == null) && (verifiedP2PNodeMap.get(receivedNode.getIPAddress()) == null)) {
                        unverifiedP2PNodeList.add(nodeFromHandshake);
                        unverifiedP2PNodeMap.put(nodeFromHandshake.getIPAddress(), nodeFromHandshake);
                    }
                }
            }
            SaveNodeList();
        }
    }

    /**
     * When we receive a handshake request return a handshake response and
     * add the node data for node that sent the handshake request
     *
     * @param handshake the handshake that was received
     */
    public P2PHandshake OnReceiveHandshakeRequest(P2PHandshake handshake) {
        ProcessHandshake(handshake.getThisNode(), handshake.getKnownNodesList(), false);
        return new P2PHandshake(
                handshake.getThisNode(),
                this.node,
                GetVerifiedNodeListSubset()
        );
    }

    /**
     * When we receive a handshake response we update its data and we now know that it is connectable
     *
     * @param handshake the handshake that was received
     */
    public void OnReceiveHandshakeResponse(P2PHandshake handshake) {
        ProcessHandshake(handshake.getThisNode(), handshake.getKnownNodesList(), true);
    }

    /**
     * Creates a P2PHeartbeat for the target node
     *
     * @param targetNode The node the heartbeat is to sent too
     * @return A P2PHeartbeat object
     */
    public P2PHeartbeat GenHeartbeatForNode(P2PNode targetNode) {
        return new P2PHeartbeat(targetNode, this.node);
    }

    /**
     * When we get a heatbeat we need to update the lastSeenTime for that node and the connection info
     *
     * @param heartbeat the heartbeat we received
     */
    public void OnReceiveHeartbeat(P2PHeartbeat heartbeat) {
        UpdateNodeLists(heartbeat.getThisNode());
    }

    /**
     * When a connection has been established with a node we need to update the is_connected parameter
     * This connection could be from a client connecting to this node or from this node connecting to a p2p node
     *
     * @param nodesIp Ip of the node we have just connected too
     */
    public void OnConnectionEstablishedWithNode(InetSocketAddress nodesIp) {
        P2PNode connectedNode = null;
        if (verifiedP2PNodeMap.get(nodesIp) != null) {
            connectedNode = verifiedP2PNodeMap.get(nodesIp);
        } else if (unverifiedP2PNodeMap.get(nodesIp) != null) {
            connectedNode = unverifiedP2PNodeMap.get(nodesIp);
        }
        if (connectedNode != null) {
            connectedNode.setConnectedToNode(true);
            // Note we don't update the nodes active connection data as we
            // don't know if this node is a p2p node or a client node
            if (connectedNode.isConnectable()) {
                if(!this.node.getConnectedP2PNodes().contains(connectedNode)) {
                    this.node.AddConnectedP2PNode(connectedNode);
                }
            } else {
                if(!this.node.getConnectedClientNodes().contains(connectedNode)) {
                    this.node.AddConnectedClientNode(connectedNode);
                }
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
    public void OnDisconnectedFromNode(InetSocketAddress nodesIp) {
        P2PNode connectedNode = null;
        if (verifiedP2PNodeMap.get(nodesIp) != null) {
            connectedNode = verifiedP2PNodeMap.get(nodesIp);
        } else if (unverifiedP2PNodeMap.get(nodesIp) != null) {
            connectedNode = unverifiedP2PNodeMap.get(nodesIp);
        }
        if (connectedNode != null) {
            connectedNode.setConnectedToNode(false);
            // Note we don't update the nodes active connection data as we
            // don't know if this node is a p2p node or a client node
            if (connectedNode.isConnectable()) {
                this.node.RemoveConnectedP2PNode(connectedNode);
            } else {
                this.node.RemoveConnectedClientNode(connectedNode);
            }
        }
    }

    /**
     * Main Function!
     * Randomly select 10 nodes to connect to
     * @return
     */
    public ArrayList<P2PNode> SelectNodesToConnect() {
        int maxIdx = verifiedP2PNodeList.size() - 1;
        ArrayList<P2PNode> nodeToConnectTo = new ArrayList<>(NUMBER_NODES_TO_CONNECT_TO);
        ThreadLocalRandom.current().ints(0, maxIdx).distinct().limit(NUMBER_NODES_TO_CONNECT_TO).forEach(
                x -> nodeToConnectTo.add(verifiedP2PNodeList.get(x))
        );
        return nodeToConnectTo;
    }

    public void RemoveUnresponsiveNodes() {
        long oldestAllowedTimestamp = System.currentTimeMillis() - TimeUnit.MINUTES.toMillis(TIME_UNTIL_NODE_REMOVED_MINS);
        long oldestNodeTimestamp = verifiedP2PNodeList.get(0).getLastSeenTimestamp();
        while (oldestNodeTimestamp < oldestAllowedTimestamp){
            P2PNode oldestNode = verifiedP2PNodeList.remove(0);
            oldestNodeTimestamp =  verifiedP2PNodeList.get(0).getLastSeenTimestamp();
            this.node.getConnectedP2PNodes().remove(oldestNode);
            this.node.getConnectedClientNodes().remove(oldestNode);
        }
        oldestNodeTimestamp = unverifiedP2PNodeList.get(0).getLastSeenTimestamp();
        while (oldestNodeTimestamp < oldestAllowedTimestamp){
            P2PNode oldestNode = unverifiedP2PNodeList.remove(0);
            oldestNodeTimestamp =  unverifiedP2PNodeList.get(0).getLastSeenTimestamp();
            this.node.getConnectedP2PNodes().remove(oldestNode);
            this.node.getConnectedClientNodes().remove(oldestNode);
        }
    }

    public P2PNode DrawNewNode() {
        return verifiedP2PNodeList.get(0);
    }

    public boolean IsNetworkHealthy() {
        return true;
    }

}
