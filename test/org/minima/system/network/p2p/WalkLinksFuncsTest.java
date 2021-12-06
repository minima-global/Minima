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


    public void testRemoveIPsInBFromA() throws UnknownHostException {
        Map<String, InetSocketAddress> A = QuickInetLists.generateConnectionUIDMap("filter", "10.0.0.", 10, 9001);
        List<InetSocketAddress> B = QuickInetLists.generateInetSockAddrList("10.0.0.", 5, 9001);
        List<InetSocketAddress> ret = WalkLinksFuncs.removeIPsInBFromA(A, B);
        assertEquals(5, ret.size());
    }

    public void testRemoveIPsInBFromAEmptyA() throws UnknownHostException {
        HashMap<String, InetSocketAddress> A = new HashMap<>();
        List<InetSocketAddress> B = QuickInetLists.generateInetSockAddrList("10.0.0.", 5, 9001);
        List<InetSocketAddress> ret = WalkLinksFuncs.removeIPsInBFromA(A, B);
        assertTrue(ret.isEmpty());
    }

    public void testOnReturnedWalkMsgOutLinksFull() throws UnknownHostException {
        P2PState state = QuickState.stateInAndOutLinks(5, 5, 9001);
        state.setMyMinimaAddress("60.0.0.1");
        List<InetSocketAddress> pathTaken = QuickInetLists.generateInetSockAddrList("60.0.0.", 10, 9001);
        P2PWalkLinks msgWalkLinks = new P2PWalkLinks(true, false, pathTaken);
        List<Message> address = WalkLinksFuncs.onReturnedWalkMsg(state, msgWalkLinks, 5);
        assertTrue(address.isEmpty());
    }

}