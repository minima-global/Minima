package org.minima.system.network.p2p;

import junit.framework.TestCase;
import org.minima.system.network.minima.NIOClientInfo;
import org.minima.system.network.p2p.messages.InetSocketAddressIO;
import org.minima.system.network.p2p.messages.P2PWalkLinks;
import org.minima.system.network.p2p.testingutils.QuickClients;
import org.minima.system.network.p2p.testingutils.QuickInetLists;
import org.minima.system.network.p2p.testingutils.QuickState;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

import java.net.InetSocketAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class WalkLinksFuncsTest extends TestCase {

    public void setUp() throws Exception {
        super.setUp();
    }

    public void tearDown() throws Exception {
    }

    public void testOnInLinkWalkMsgNextHop() throws UnknownHostException{
        List<NIOClientInfo> allClients = QuickClients.generateClientInfoList("60.0.0.", 5,9001, "inLinkUID", true);
        int tgtNumLinks = 5;
        NIOClientInfo clientMsgIsFrom = new NIOClientInfo("uid1", "192.168.0.1", 9001, true);

        // InLinks inLinkUID 60.0.0. OutLinks outLinkUID 70.0.0.
        P2PState state = QuickState.stateInAndOutLinks(5, 5, 9001);

        P2PWalkLinks p2pWalkLinks = new P2PWalkLinks(true, true, "inLinkUID2");
        p2pWalkLinks.setPathTaken(QuickInetLists.generateInetSockAddrList("60.0.0.", 2, 9001));

        Message retMsg = WalkLinksFuncs.onInLinkWalkMsg(state, p2pWalkLinks, clientMsgIsFrom, tgtNumLinks, allClients);
        assertNotNull(retMsg);
        String uid = retMsg.getString("uid");
        JSONObject json = (JSONObject) retMsg.getObject("json");
        assertTrue(json.containsKey("walk_links"));
        assertTrue(uid.contains("inLinkUID"));
    }

    public void testOnInLinkWalkMsgEndOfPath() throws UnknownHostException {
        List<NIOClientInfo> allClients = QuickClients.generateClientInfoList("60.0.0.", 20,9001, "inLinkUID", true);
        int tgtNumLinks = 5;
        NIOClientInfo clientMsgIsFrom = new NIOClientInfo("inLinkUID5", "60.0.0.9", 9001, true);

        // InLinks inLinkUID 60.0.0. OutLinks outLinkUID 70.0.0.
        P2PState state = QuickState.stateInAndOutLinks(5, 10, 9001);
        state.setMyMinimaAddress("60.0.0.10");
        P2PWalkLinks p2pWalkLinks = new P2PWalkLinks(false, false, "outLinkUID9");
        p2pWalkLinks.setPathTaken(QuickInetLists.generateInetSockAddrList("60.0.0.", 9, 9001));

        Message retMsg = WalkLinksFuncs.onInLinkWalkMsg(state, p2pWalkLinks, clientMsgIsFrom, tgtNumLinks, allClients);
        assertNotNull(retMsg);
        String uid = retMsg.getString("uid");
        JSONObject json = (JSONObject) retMsg.getObject("json");
        assertTrue(json.containsKey("walk_links"));
        assertTrue(uid.contains("inLinkUID"));
    }

    public void testOnOutLinkWalkMsgNextHop() throws UnknownHostException {
        List<NIOClientInfo> allClients = QuickClients.generateClientInfoList("70.0.0.", 5,9001, "outLinkUID", true);
        int tgtNumLinks = 5;
        NIOClientInfo clientMsgIsFrom = new NIOClientInfo("uid1", "192.168.0.1", 9001, true);

        // InLinks inLinkUID 60.0.0. OutLinks outLinkUID 70.0.0.
        P2PState state = QuickState.stateInAndOutLinks(5, 5, 9001);

        P2PWalkLinks p2pWalkLinks = new P2PWalkLinks(false, false, "inLinkUID2");
        p2pWalkLinks.setPathTaken(QuickInetLists.generateInetSockAddrList("70.0.0.", 2, 9001));

        Message retMsg = WalkLinksFuncs.onOutLinkWalkMsg(state, p2pWalkLinks, clientMsgIsFrom, tgtNumLinks, allClients);
        assertNotNull(retMsg);
        String uid = retMsg.getString("uid");
        JSONObject json = (JSONObject) retMsg.getObject("json");
        assertTrue(json.containsKey("walk_links"));
        assertTrue(uid.contains("outLinkUID"));
    }

    public void testOnOutLinkWalkMsgDoSwapEndOfPath() throws UnknownHostException {
        List<NIOClientInfo> allClients = QuickClients.generateClientInfoList("70.0.0.", 20,9001, "outLinkUID", true);
        int tgtNumLinks = 5;
        NIOClientInfo clientMsgIsFrom = new NIOClientInfo("uid1", "192.168.0.1", 9001, true);

        // InLinks inLinkUID 60.0.0. OutLinks outLinkUID 70.0.0.
        P2PState state = QuickState.stateInAndOutLinks(5, 10, 9001);

        P2PWalkLinks p2pWalkLinks = new P2PWalkLinks(false, false, "inLinkUID9");
        p2pWalkLinks.setPathTaken(QuickInetLists.generateInetSockAddrList("70.0.0.", 9, 9001));

        Message retMsg = WalkLinksFuncs.onOutLinkWalkMsg(state, p2pWalkLinks, clientMsgIsFrom, tgtNumLinks, allClients);
        assertNotNull(retMsg);
        String uid = retMsg.getString("uid");
        JSONObject json = (JSONObject) retMsg.getObject("json");
        assertTrue(json.containsKey("do_swap"));
        assertTrue(uid.contains("inLinkUID"));
    }

    public void testOnOutLinkWalkMsgDoSwapNoNewLinks() throws UnknownHostException {
        List<NIOClientInfo> allClients = QuickClients.generateClientInfoList("70.0.0.", 5,9001, "outLinkUID", true);
        int tgtNumLinks = 5;
        NIOClientInfo clientMsgIsFrom = new NIOClientInfo("uid1", "192.168.0.1", 9001, true);

        // InLinks inLinkUID 60.0.0. OutLinks outLinkUID 70.0.0.
        P2PState state = QuickState.stateInAndOutLinks(5, 10, 9001);

        P2PWalkLinks p2pWalkLinks = new P2PWalkLinks(false, false, "inLinkUID9");
        p2pWalkLinks.setPathTaken(QuickInetLists.generateInetSockAddrList("70.0.0.", 5, 9001));

        Message retMsg = WalkLinksFuncs.onOutLinkWalkMsg(state, p2pWalkLinks, clientMsgIsFrom, tgtNumLinks, allClients);
        assertNotNull(retMsg);
        String uid = retMsg.getString("uid");
        JSONObject json = (JSONObject) retMsg.getObject("json");
        assertTrue(json.containsKey("do_swap"));
        assertTrue(uid.contains("inLinkUID"));
    }

    public void testCreateNextHopMsg() throws UnknownHostException {
        List<NIOClientInfo> allClients =QuickClients.generateClientInfoList("10.0.0.", 5,9001, "nectHopUID", true);
        InetSocketAddress nextHop = new InetSocketAddress("10.0.0.2", 9001);
        List<InetSocketAddress> pathTaken = QuickInetLists.generateInetSockAddrList("13.13.13.", 9, 9001);

        P2PWalkLinks msgWalkLinks = new P2PWalkLinks(true, false, pathTaken);

        Message retMsg = WalkLinksFuncs.createNextHopMsg(nextHop, msgWalkLinks, allClients);
        String uid = retMsg.getString("uid");
        JSONObject json = (JSONObject) retMsg.getObject("json");
        assertFalse(uid.isEmpty());
        assertTrue(json.containsKey("walk_links"));
    }

    public void testCreateNextHopMsgClientNotFound() throws UnknownHostException {
        List<NIOClientInfo> allClients =QuickClients.generateClientInfoList("10.0.0.", 5,9001, "nectHopUID", true);
        InetSocketAddress nextHop = new InetSocketAddress("11.0.0.2", 9001);
        List<InetSocketAddress> pathTaken = QuickInetLists.generateInetSockAddrList("13.13.13.", 9, 9001);

        P2PWalkLinks msgWalkLinks = new P2PWalkLinks(true, false, pathTaken);

        Message retMsg = WalkLinksFuncs.createNextHopMsg(nextHop, msgWalkLinks, allClients);
        assertNull(retMsg);

    }


    public void testGenerateDoSwapMessageFromWalkNotEnoughInLinks() throws UnknownHostException {
        P2PState state = QuickState.stateInAndOutLinks(5, 2, 9100);
        List<InetSocketAddress> pathTaken = QuickInetLists.generateInetSockAddrList("13.13.13.", 9, 9001);
        P2PWalkLinks msgWalkLinks = new P2PWalkLinks(true, false, pathTaken);
        int tgtNumLinks = 5;

        Message returnMsg = WalkLinksFuncs.generateDoSwapMessageFromWalk(state, msgWalkLinks, tgtNumLinks);
        assertNull(returnMsg);
    }


    public void testGenerateDoSwapMessageFromWalk() throws UnknownHostException {
        P2PState state = QuickState.stateInAndOutLinks(5, 10, 9100);
        List<InetSocketAddress> pathTaken = QuickInetLists.generateInetSockAddrList("13.13.13.", 9, 9001);
        P2PWalkLinks msgWalkLinks = new P2PWalkLinks(true, false, pathTaken);
        int tgtNumLinks = 5;

        Message returnMsg = WalkLinksFuncs.generateDoSwapMessageFromWalk(state, msgWalkLinks, tgtNumLinks);
        String uid = returnMsg.getString("uid");
        JSONObject json = (JSONObject) returnMsg.getObject("json");
        assertFalse(uid.isEmpty());
        assertTrue(json.containsKey("do_swap"));
    }


    public void testRemoveIPsInBFromA() throws UnknownHostException{
        Map<String, InetSocketAddress> A = QuickInetLists.generateConnectionUIDMap("filter", "10.0.0.", 10, 9001);
        List<InetSocketAddress> B = QuickInetLists.generateInetSockAddrList("10.0.0.", 5, 9001);
        List<InetSocketAddress> ret = WalkLinksFuncs.removeIPsInBFromA(A, B);
        assertEquals(5, ret.size());
    }

    public void testRemoveIPsInBFromAEmptyA() throws UnknownHostException{
        HashMap<String, InetSocketAddress> A = new HashMap<>();
        List<InetSocketAddress> B = QuickInetLists.generateInetSockAddrList("10.0.0.", 5, 9001);
        List<InetSocketAddress> ret = WalkLinksFuncs.removeIPsInBFromA(A, B);
        assertTrue(ret.isEmpty());
    }


    public void testOnWalkLinkResponseMsg() throws UnknownHostException{
        List<NIOClientInfo> allClients = QuickClients.generateClientInfoList("60.0.0.", 5,9001, "nextHopUID", true);
        P2PState state = QuickState.stateInAndOutLinks(5, 5, 9001);
        state.setMyMinimaAddress("60.0.0.3");
        List<InetSocketAddress> pathTaken = QuickInetLists.generateInetSockAddrList("60.0.0.", 9, 9001);

        P2PWalkLinks msgWalkLinks = new P2PWalkLinks(true, false, pathTaken);

        Message retMsg = WalkLinksFuncs.onWalkLinkResponseMsg(state, msgWalkLinks, allClients);
        String uid = retMsg.getString("uid");
        JSONObject json = (JSONObject) retMsg.getObject("json");
        assertFalse(uid.isEmpty());
        assertTrue(json.containsKey("walk_links"));
    }

    public void testOnReturnedWalkMsg() throws UnknownHostException{
        P2PState state = QuickState.stateInAndOutLinks(4, 5, 9001);
        state.setMyMinimaAddress("60.0.0.1");
        List<InetSocketAddress> pathTaken = QuickInetLists.generateInetSockAddrList("60.0.0.", 10, 9001);
        P2PWalkLinks msgWalkLinks = new P2PWalkLinks(true, false, pathTaken);
        InetSocketAddress address = WalkLinksFuncs.onReturnedWalkMsg(state, msgWalkLinks, 5);
        assertNotNull(address);
        assertEquals(11, state.getKnownPeers().size());
    }

    public void testOnReturnedWalkMsgConnectingToSelf() throws UnknownHostException{
        P2PState state = QuickState.stateInAndOutLinks(4, 5, 9001);
        state.setMyMinimaAddress("60.0.0.1");
        List<InetSocketAddress> pathTaken = QuickInetLists.generateInetSockAddrList("60.0.0.", 1, 9001);
        P2PWalkLinks msgWalkLinks = new P2PWalkLinks(true, false, pathTaken);
        InetSocketAddress address = WalkLinksFuncs.onReturnedWalkMsg(state, msgWalkLinks, 5);
        assertNull(address);
    }

    public void testOnReturnedWalkMsgOutLinksFull() throws UnknownHostException{
        P2PState state = QuickState.stateInAndOutLinks(5, 5, 9001);
        state.setMyMinimaAddress("60.0.0.1");
        List<InetSocketAddress> pathTaken = QuickInetLists.generateInetSockAddrList("60.0.0.", 10, 9001);
        P2PWalkLinks msgWalkLinks = new P2PWalkLinks(true, false, pathTaken);
        InetSocketAddress address = WalkLinksFuncs.onReturnedWalkMsg(state, msgWalkLinks, 5);
        assertNull(address);
    }

}