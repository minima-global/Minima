package org.minima.system.network.p2p;

import junit.framework.TestCase;
import org.junit.Rule;
import org.junit.rules.ExpectedException;
import org.minima.objects.base.MiniData;
import org.minima.system.network.minima.NIOClientInfo;
import org.minima.system.network.p2p.messages.P2PGreeting;
import org.minima.system.network.p2p.testingutils.QuickClients;
import org.minima.system.network.p2p.testingutils.QuickInetLists;
import org.minima.system.network.p2p.testingutils.QuickState;
import org.minima.system.params.GeneralParams;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

import java.net.InetSocketAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

public class SwapLinksFunctionsTest extends TestCase {

    final String noConnectionsGreetingJson = "{\"greeting\":{\"myMinimaPort\":9001,\"isAcceptingInLinks\":true,\"numNoneP2PConnections\":1,\"maxNumNoneP2PConnections\":50,\"outLinks\":[],\"inLinks\":[],\"knownPeers\":[{\"host\":\"192.168.0.1\",\"port\":9001},{\"host\":\"10.0.0.1\",\"port\":9001}]}}";
    Message inConnectMsg;
    Message outConnectMsg;

    public void setUp() throws Exception {
        super.setUp();
        inConnectMsg = new Message().addString("uid", "uid1").addBoolean("incoming", true);
        outConnectMsg = new Message().addString("uid", "uid1").addBoolean("incoming", false);
    }

    public void testOnDisconnectedKnownPeer() throws UnknownHostException {
        P2PState state = QuickState.stateInLinkKnownPeerOnly();
        Message msg = new Message().addString("uid", "knownPeer").addBoolean("incoming", true).addBoolean("reconnect", false);
        SwapLinksFunctions.onDisconnected(state, msg);
        assertFalse(state.getKnownPeers().isEmpty());
    }


    public void testOnDisconnectedInLink() throws UnknownHostException {
        P2PState state = QuickState.stateInLinksOnly(1, 0);
        Message msg = new Message().addString("uid", "inLinkUID1").addBoolean("incoming", true).addBoolean("reconnect", false);
        SwapLinksFunctions.onDisconnected(state, msg);
        assertTrue(state.getInLinks().isEmpty());
    }

    public void testOnDisconnectedOutLink() throws UnknownHostException {
        P2PState state = QuickState.stateOutLinksOnly(1);
        Message msg = new Message().addString("uid", "outLinkUID1").addBoolean("incoming", false).addBoolean("reconnect", false);
        SwapLinksFunctions.onDisconnected(state, msg);
        assertTrue(state.getOutLinks().isEmpty());

    }

    public void testOnDisconnectedNoneP2PLink() throws UnknownHostException {
        P2PState state = QuickState.stateNoneP2PLink(1, 0);
        Message msg = new Message().addString("uid", "noneP2PUID1").addBoolean("incoming", true).addBoolean("reconnect", false);
        SwapLinksFunctions.onDisconnected(state, msg);
        assertTrue(state.getNoneP2PLinks().isEmpty());
    }

    public void testProcessGreetingInLink() throws UnknownHostException {
        NIOClientInfo client = new NIOClientInfo("noneP2PUID1", "10.0.1.1", 9001, true);
        P2PState greetingState = QuickState.stateInAndOutLinks(2, 3, 0);
        greetingState.setMyMinimaAddress("10.0.1.1");
        P2PGreeting greeting = new P2PGreeting(greetingState);

        P2PState state = QuickState.stateNoneP2PLink(1, 9001);
        SwapLinksFunctions.processGreeting(state, greeting, client, true);

        assertFalse(state.getInLinks().isEmpty());
        assertTrue(state.getNoneP2PLinks().isEmpty());

    }


    public void testProcessGreetingOutLink() throws UnknownHostException {
        NIOClientInfo client = new NIOClientInfo("noneP2PUID1", "10.0.1.1", 9001, false);
        P2PState greetingState = QuickState.stateInAndOutLinks(2, 3, 0);
        greetingState.setMyMinimaAddress("10.0.1.1");
        P2PGreeting greeting = new P2PGreeting(greetingState);

        P2PState state = QuickState.stateNoneP2PLink(1, 9001);
        SwapLinksFunctions.processGreeting(state, greeting, client, true);

        assertFalse(state.getOutLinks().isEmpty());
        assertTrue(state.getNoneP2PLinks().isEmpty());
    }

    public void testProcessRequestIPMsg() {
        JSONObject json = new JSONObject();
        String secret = MiniData.getRandomData(12).toString();
        json.put("req_ip", secret);
        JSONObject respMsg = SwapLinksFunctions.processRequestIPMsg(json, "147.258.369.1");
        assertEquals(secret, (String) ((JSONObject) respMsg.get("swap_links_p2p")).get("secret"));
    }

    public void testProcessResponseIPMsgIncorrectSecret() throws UnknownHostException {
        JSONObject json = new JSONObject();
        MiniData secret = MiniData.getRandomData(12);
        P2PState state = QuickState.stateInAndOutLinks(1, 2, 0);

        json.put("req_ip", secret.toString());
        JSONObject respMsg = SwapLinksFunctions.processRequestIPMsg(json, "147.258.369.1");
        respMsg = (JSONObject) respMsg.get("swap_links_p2p");

        SwapLinksFunctions.processResponseIPMsg(state, respMsg);

        assertEquals(new InetSocketAddress("10.0.0.1", GeneralParams.MINIMA_PORT), state.getMyMinimaAddress());
    }


}