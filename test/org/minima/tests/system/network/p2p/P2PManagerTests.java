package org.minima.tests.system.network.p2p;
import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.minima.Start;
import org.minima.system.network.p2p.P2PHandshake;
import org.minima.system.network.p2p.P2PHeartbeat;
import org.minima.system.network.p2p.P2PManager;
import org.minima.system.network.p2p.P2PNode;

import java.io.File;
import java.lang.reflect.Field;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.HashMap;

import static org.junit.Assert.*;
import static org.junit.Assert.assertEquals;

public class P2PManagerTests {

    P2PManager p2p;

    @Before
    public void before() throws Exception {
        InetAddress ip = InetAddress.getLocalHost();
        File dataFile = new File("p2pdatafile.json");
        ArrayList<P2PNode> testNodeList = new ArrayList<>();

        for(int ii = 1; ii < 5; ii++) {
            for (int i = 0; i < 250; i++) {
                testNodeList.add(new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.".concat(String.valueOf(ii)).concat(".").concat(String.valueOf(i))), 9001), 0, null, null, false, false));
            }
        }

        p2p = new P2PManager(testNodeList, new InetSocketAddress(ip, 9001), dataFile);

    }

    @Test
    public void testInitLoadDefaults() throws Exception {
        InetAddress ip = InetAddress.getLocalHost();
        File dataFile = new File("p2pdatafile.json");
        P2PManager p2pLoadTester = new P2PManager(new InetSocketAddress(ip, 9001), dataFile);

        assertEquals(p2pLoadTester.getUnverifiedP2PNodeList().size(), Start.VALID_BOOTSTRAP_NODES.length);
    }

    @Test
    public void testInitPassList() throws Exception {
        assertEquals(p2p.getUnverifiedP2PNodeList().size(), 1000);
    }

    @Test
    public void testSaveLoadNodeList() throws Exception {

        ArrayList<P2PNode> testNodeList = new ArrayList<>();
        testNodeList.add(new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001), 0, null, null, false, false));
        testNodeList.add(new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.2"), 9001), 0, null, null, false, false));
        testNodeList.add(new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.3"), 9001), 0, null, null, false, false));

        // Set private field
        Field reader = P2PManager.class.getDeclaredField("verifiedP2PNodeList");
        reader.setAccessible(true);
        reader.set(p2p, testNodeList);

        assertEquals(p2p.getVerifiedP2PNodeList().size(), 3);
        p2p.SaveNodeList();
        p2p.LoadNodeList();

        assertEquals(p2p.getUnverifiedP2PNodeList().size(), 3);
    }

    @Test
    public void testGenHandshakeWithNodeList() throws Exception {
        ArrayList<P2PHandshake> handshakes = p2p.GenHandshakeWithUnverifiedNodes();

        assertEquals(handshakes.size(), 1000);
        for(int i =0; i < p2p.getUnverifiedP2PNodeList().size(); i++){
            assertEquals(handshakes.get(i).getTargetNode().getIPAddress(), p2p.getUnverifiedP2PNodeList().get(i).getIPAddress());
        }
    }

    @Test
    public void testGenHandshakeForNode() throws Exception {
        ArrayList<P2PNode> testNodeList = new ArrayList<>();
        testNodeList.add(new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001), 0, null, null, false, false));
        testNodeList.add(new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.2"), 9001), 0, null, null, false, false));
        testNodeList.add(new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.3"), 9001), 0, null, null, false, false));

        P2PNode targetNode = new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001), 0, null, null, false, false);


        // Set private field
        Field reader = P2PManager.class.getDeclaredField("verifiedP2PNodeList");
        reader.setAccessible(true);
        reader.set(p2p, testNodeList);


        ArrayList<P2PNode> connectedP2PNodes = new ArrayList<>();
        connectedP2PNodes.add(new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001), 0, null, null, true, true));
        ArrayList<P2PNode> connectedClientNodes = new ArrayList<>();
        connectedClientNodes.add(new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.2"), 9001), 0, null, null, true, false));

        p2p.getNode().setConnectedP2PNodes(connectedP2PNodes);
        p2p.getNode().setConnectedClientNodes(connectedClientNodes);

        P2PHandshake handshake = p2p.GenHandshakeForNode(targetNode);
        assertEquals(handshake.getKnownNodesList().size(), 3);
        assertEquals(handshake.getThisNode().getIPAddress(), new InetSocketAddress(InetAddress.getLocalHost(), 9001));
        assertEquals(handshake.getThisNode().getConnectedP2PNodes().size(), 1);
        assertEquals(handshake.getThisNode().getConnectedP2PNodes().get(0).getIPAddress(), new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001));
        assertEquals(handshake.getThisNode().getConnectedClientNodes().size(), 1);
        assertEquals(handshake.getThisNode().getConnectedClientNodes().get(0).getIPAddress(), new InetSocketAddress(InetAddress.getByName("192.168.0.2"), 9001));

        assertEquals(handshake.getTargetNode().getIPAddress(), targetNode.getIPAddress());
    }

    @Test
    public void testGetVerifiedNodeListSubset() throws Exception {

        // Set private field
        Field reader = P2PManager.class.getDeclaredField("verifiedP2PNodeList");
        reader.setAccessible(true);
        reader.set(p2p, p2p.getUnverifiedP2PNodeList());

        ArrayList<P2PNode> nodeListSubset = p2p.GetVerifiedNodeListSubset();

        assertEquals(nodeListSubset.size(), 200);
    }

    @Test
    public void testUpdateNodeListForVerifiedNode() throws Exception {
        ArrayList<P2PNode> connectedP2PNodes = new ArrayList<>();
        connectedP2PNodes.add(new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001), 0, null, null, true, true));
        ArrayList<P2PNode> connectedClientNodes = new ArrayList<>();
        connectedClientNodes.add(new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.2"), 9001), 0, null, null, true, false));

        P2PNode testNode = new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001), 0, null, null, true, true);
        ArrayList<P2PNode> testNodeList = new ArrayList<>();
        testNodeList.add(testNode);

        HashMap<InetSocketAddress, P2PNode> verifiedP2PNodeMap = new HashMap<>();
        verifiedP2PNodeMap.put(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001), testNode);
        // Set private field
        Field reader = P2PManager.class.getDeclaredField("verifiedP2PNodeList");
        reader.setAccessible(true);
        reader.set(p2p, testNodeList);

        Field readerMap = P2PManager.class.getDeclaredField("verifiedP2PNodeMap");
        readerMap.setAccessible(true);
        readerMap.set(p2p, verifiedP2PNodeMap);

        P2PNode verifiedNodeToCheck = p2p.getVerifiedP2PNodeMap().get(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001));
        assertNotNull(verifiedNodeToCheck);
        assertTrue(verifiedNodeToCheck.getConnectedClientNodes().isEmpty());
        assertTrue(verifiedNodeToCheck.getConnectedP2PNodes().isEmpty());
        assertEquals(verifiedNodeToCheck.getLastSeenTimestamp(), 0);

        p2p.UpdateNodeLists(new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001), 0, connectedP2PNodes, connectedClientNodes, true, true));

        assertNotNull(verifiedNodeToCheck.getConnectedClientNodes());
        assertNotNull(verifiedNodeToCheck.getConnectedP2PNodes());
        assertNotEquals(verifiedNodeToCheck.getLastSeenTimestamp(), 0);
    }

    @Test
    public void testUpdateNodeListForUnverifiedNode() throws Exception {
        ArrayList<P2PNode> connectedP2PNodes = new ArrayList<>();
        connectedP2PNodes.add(new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001), 0, null, null, true, true));
        ArrayList<P2PNode> connectedClientNodes = new ArrayList<>();
        connectedClientNodes.add(new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.2"), 9001), 0, null, null, true, false));

        P2PNode testNode = new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001), 0, null, null, true, true);
        ArrayList<P2PNode> testNodeList = new ArrayList<>();
        testNodeList.add(testNode);

        InetAddress ip = InetAddress.getLocalHost();
        File dataFile = new File("p2pdatafile.json");
        P2PManager p2p = new P2PManager(testNodeList, new InetSocketAddress(ip, 9001), dataFile);

        P2PNode unverifiedNodeToCheck = p2p.getUnverifiedP2PNodeMap().get(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001));
        assertNotNull(unverifiedNodeToCheck);
        assertTrue(unverifiedNodeToCheck.getConnectedClientNodes().isEmpty());
        assertTrue(unverifiedNodeToCheck.getConnectedP2PNodes().isEmpty());
        assertEquals(unverifiedNodeToCheck.getLastSeenTimestamp(), 0);

        p2p.UpdateNodeLists(new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001), 0, connectedP2PNodes, connectedClientNodes, true, true));

        P2PNode verifiedNodeToCheck = p2p.getVerifiedP2PNodeMap().get(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001));
        assertNotNull(verifiedNodeToCheck.getConnectedClientNodes());
        assertNotNull(verifiedNodeToCheck.getConnectedP2PNodes());
        assertNotEquals(verifiedNodeToCheck.getLastSeenTimestamp(), 0);
        assertEquals(p2p.getUnverifiedP2PNodeList().size(), 0);
        assertEquals(p2p.getUnverifiedP2PNodeMap().size(), 0);
    }

    @Test
    public void testUpdateNodeListForNewNode() throws Exception {
        ArrayList<P2PNode> connectedP2PNodes = new ArrayList<>();
        connectedP2PNodes.add(new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001), 0, null, null, true, true));
        ArrayList<P2PNode> connectedClientNodes = new ArrayList<>();
        connectedClientNodes.add(new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.2"), 9001), 0, null, null, true, false));

        P2PNode testNode = new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001), 0, null, null, true, true);
        ArrayList<P2PNode> testNodeList = new ArrayList<>();
        testNodeList.add(testNode);

        P2PNode unverifiedNodeToCheck = p2p.getUnverifiedP2PNodeMap().get(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001));
        assertNull(unverifiedNodeToCheck);

        p2p.UpdateNodeLists(new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001), 0, connectedP2PNodes, connectedClientNodes, true, true));

        unverifiedNodeToCheck = p2p.getUnverifiedP2PNodeMap().get(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001));
        assertNotNull(unverifiedNodeToCheck.getConnectedClientNodes());
        assertNotNull(unverifiedNodeToCheck.getConnectedP2PNodes());
        assertNotEquals(unverifiedNodeToCheck.getLastSeenTimestamp(), 0);
        assertEquals(p2p.getVerifiedP2PNodeMap().size(), 0);
        assertEquals(p2p.getVerifiedP2PNodeList().size(), 0);
    }

    @Test
    public void testProcessResponseHandshake() throws Exception {
        // Note Only testing Response path as if it's not a response it just invokes
        // UpdateNodeList which is tested by the previous 3 tests
        ArrayList<P2PNode> connectedP2PNodes = new ArrayList<>();
        connectedP2PNodes.add(new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001), 0, null, null, true, true));
        ArrayList<P2PNode> connectedClientNodes = new ArrayList<>();
        connectedClientNodes.add(new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.2"), 9001), 0, null, null, true, false));

        P2PNode testNode = new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001), 0, null, null, true, true);
        ArrayList<P2PNode> testNodeList = new ArrayList<>();
        testNodeList.add(testNode);

        InetAddress ip = InetAddress.getLocalHost();
        File dataFile = new File("p2pdatafile.json");
        P2PManager p2p = new P2PManager(testNodeList, new InetSocketAddress(ip, 9001), dataFile);

        P2PNode unverifiedNodeToCheck = p2p.getUnverifiedP2PNodeMap().get(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001));
        assertNotNull(unverifiedNodeToCheck);
        assertTrue(unverifiedNodeToCheck.getConnectedClientNodes().isEmpty());
        assertTrue(unverifiedNodeToCheck.getConnectedP2PNodes().isEmpty());
        assertEquals(unverifiedNodeToCheck.getLastSeenTimestamp(), 0);

        ArrayList<P2PNode> receivedNodeList = new ArrayList<>();
        for(int i = 0; i < 255; i++){
            testNodeList.add(new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.".concat(String.valueOf(i))), 9001), 0, null, null, false, false));
        }

        p2p.ProcessHandshake(new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001), 0, connectedP2PNodes, connectedClientNodes, true, true), receivedNodeList, true);

        P2PNode verifiedNodeToCheck = p2p.getVerifiedP2PNodeMap().get(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001));
        assertNotNull(verifiedNodeToCheck.getConnectedClientNodes());
        assertNotNull(verifiedNodeToCheck.getConnectedP2PNodes());
        assertNotEquals(verifiedNodeToCheck.getLastSeenTimestamp(), 0);
        assertTrue(verifiedNodeToCheck.isConnectable());
        assertEquals(p2p.getUnverifiedP2PNodeList().size(), 255);
        assertEquals(p2p.getUnverifiedP2PNodeMap().size(), 0);
    }

    @Test
    public void testOnReceiveHandshakeRequest() throws Exception {
        // Set private field
        Field reader = P2PManager.class.getDeclaredField("verifiedP2PNodeList");
        reader.setAccessible(true);
        reader.set(p2p, p2p.getUnverifiedP2PNodeList());

        P2PNode targetNode = new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001), 0, null, null, true, true);
        P2PHandshake handshake = new P2PHandshake(targetNode, p2p.getNode(), null);
        assertEquals(p2p.OnReceiveHandshakeRequest(handshake).getKnownNodesList().size(), 200);
    }

    @Test
    public void testOnReceiveHandshakeResponse() throws Exception {
        // Note Only testing Response path as if it's not a response it just invokes
        // UpdateNodeList which is tested by the previous 3 tests
        ArrayList<P2PNode> connectedP2PNodes = new ArrayList<>();
        connectedP2PNodes.add(new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001), 0, null, null, true, true));
        ArrayList<P2PNode> connectedClientNodes = new ArrayList<>();
        connectedClientNodes.add(new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.2"), 9001), 0, null, null, true, false));

        P2PNode testNode = new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001), 0, null, null, true, true);
        ArrayList<P2PNode> testNodeList = new ArrayList<>();
        testNodeList.add(testNode);

        InetAddress ip = InetAddress.getLocalHost();
        File dataFile = new File("p2pdatafile.json");
        P2PManager p2p = new P2PManager(testNodeList, new InetSocketAddress(ip, 9001), dataFile);

        P2PNode unverifiedNodeToCheck = p2p.getUnverifiedP2PNodeMap().get(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001));
        assertNotNull(unverifiedNodeToCheck);
        assertTrue(unverifiedNodeToCheck.getConnectedClientNodes().isEmpty());
        assertTrue(unverifiedNodeToCheck.getConnectedP2PNodes().isEmpty());
        assertEquals(unverifiedNodeToCheck.getLastSeenTimestamp(), 0);

        ArrayList<P2PNode> receivedNodeList = new ArrayList<>();
        for(int i = 0; i < 255; i++){
            testNodeList.add(new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.".concat(String.valueOf(i))), 9001), 0, null, null, false, false));
        }

        P2PHandshake handshake = new P2PHandshake(p2p.getNode(), new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001), 0, connectedP2PNodes, connectedClientNodes, true, true), receivedNodeList);

        p2p.OnReceiveHandshakeResponse(handshake);

        P2PNode verifiedNodeToCheck = p2p.getVerifiedP2PNodeMap().get(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001));
        assertNotNull(verifiedNodeToCheck.getConnectedClientNodes());
        assertNotNull(verifiedNodeToCheck.getConnectedP2PNodes());
        assertNotEquals(verifiedNodeToCheck.getLastSeenTimestamp(), 0);
        assertTrue(verifiedNodeToCheck.isConnectable());
        assertEquals(p2p.getUnverifiedP2PNodeList().size(), 255);
        assertEquals(p2p.getUnverifiedP2PNodeMap().size(), 0);
    }

    @Test
    public void testGenHeartbeatForNode() throws Exception {
        P2PNode targetNode = new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001), 0, null, null, true, true);
        P2PHeartbeat hb = p2p.GenHeartbeatForNode(targetNode);
        assertEquals(hb.getTargetNode(), targetNode);
        assertEquals(hb.getThisNode(), p2p.getNode());
    }

    @Test
    public void testOnReceiveHeartbeat() throws Exception {

        ArrayList<P2PNode> connectedP2PNodes = new ArrayList<>();
        connectedP2PNodes.add(new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001), 0, null, null, true, true));
        ArrayList<P2PNode> connectedClientNodes = new ArrayList<>();
        connectedClientNodes.add(new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.2"), 9001), 0, null, null, true, false));
        P2PNode hbNode = new P2PNode(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001), 0, connectedP2PNodes, connectedClientNodes, true, true);
        ArrayList<P2PNode> verifiedNodes = new ArrayList<>();
        verifiedNodes.add(hbNode);

        // Set private field
        Field reader = P2PManager.class.getDeclaredField("verifiedP2PNodeList");
        reader.setAccessible(true);
        reader.set(p2p, verifiedNodes);

        HashMap<InetSocketAddress, P2PNode> verifiedP2PNodeMap = new HashMap<>();
        verifiedP2PNodeMap.put(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001), hbNode);

        Field readerMap = P2PManager.class.getDeclaredField("verifiedP2PNodeMap");
        readerMap.setAccessible(true);
        readerMap.set(p2p, verifiedP2PNodeMap);

        P2PHeartbeat hb = new P2PHeartbeat(p2p.getNode(), hbNode);

        p2p.OnReceiveHeartbeat(hb);

        P2PNode verifiedNodeToCheck = p2p.getVerifiedP2PNodeMap().get(new InetSocketAddress(InetAddress.getByName("192.168.0.1"), 9001));
        assertNotNull(verifiedNodeToCheck.getConnectedClientNodes());
        assertNotNull(verifiedNodeToCheck.getConnectedP2PNodes());
        assertNotEquals(verifiedNodeToCheck.getLastSeenTimestamp(), 0);
        assertTrue(verifiedNodeToCheck.isConnectable());
        assertEquals(p2p.getUnverifiedP2PNodeList().size(), 1000);
        assertEquals(p2p.getUnverifiedP2PNodeMap().size(), 1000);
    }

    @Test
    @Ignore
    public void testOnConnectionEstablishedWithNode() throws Exception {
    }

    @Test
    @Ignore
    public void testOnDisconnectedFromNode() throws Exception {
    }

    @Test
    public void testSelectNodesToConnect() throws Exception {
        // Set private field
        Field reader = P2PManager.class.getDeclaredField("verifiedP2PNodeList");
        reader.setAccessible(true);
        reader.set(p2p, p2p.getUnverifiedP2PNodeList());

        assertEquals(p2p.SelectNodesToConnect().size(), 10);
    }


    @Test
    @Ignore
    public void testRemoveUnresponsiveNodes() throws Exception {
    }

    @Test
    public void testDrawNewNode() throws Exception {
        // Set private field
        Field reader = P2PManager.class.getDeclaredField("verifiedP2PNodeList");
        reader.setAccessible(true);
        reader.set(p2p, p2p.getUnverifiedP2PNodeList());

        assertNotNull(p2p.DrawNewNode());
    }

    @Test
    public void testIsNetworkHealthy() throws Exception {
        assertTrue(p2p.IsNetworkHealthy());
    }

    @After
    public void after() throws Exception {
        File dataFile = new File("p2pdatafile.json");
        dataFile.delete();
    }
}