package org.minima.system.network.p2p.messages;

import junit.framework.TestCase;
import org.minima.utils.json.JSONObject;

import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.util.ArrayList;

public class GreetingTest extends TestCase {

    P2PGreeting greetingAcceptingCon;
    P2PGreeting greetingNotAcceptingCon;

    static final String notAcceptingJson = "{\"greeting\":{\"myMinimaPort\":9121,\"isAcceptingInLinks\":false,\"knownPeers\":[{\"host\":\"192.168.0.1\",\"port\":9001},{\"host\":\"192.168.0.2\",\"port\":9001}]}}";
    static final String acceptingJson = "{\"greeting\":{\"myMinimaPort\":9121,\"isAcceptingInLinks\":true,\"numNoneP2PConnections\":5,\"maxNumNoneP2PConnections\":20,\"outLinks\":[{\"host\":\"10.0.0.2\",\"port\":9001},{\"host\":\"10.0.0.4\",\"port\":9001}],\"inLinks\":[{\"host\":\"10.0.0.1\",\"port\":9001},{\"host\":\"10.0.0.2\",\"port\":9001}],\"knownPeers\":[{\"host\":\"192.168.0.1\",\"port\":9001},{\"host\":\"192.168.0.2\",\"port\":9001}]}}";
    public void setUp() throws Exception {
        super.setUp();
        ArrayList<InetSocketAddress> knownPeers = new ArrayList<>();
        ArrayList<InetSocketAddress> inLinks = new ArrayList<>();
        ArrayList<InetSocketAddress> outLinks = new ArrayList<>();

        for (int i = 1; i < 3; i++) {
            knownPeers.add(new InetSocketAddress(InetAddress.getByName("192.168.0.".concat(String.valueOf(i))), 9001));
            inLinks.add(new InetSocketAddress(InetAddress.getByName("10.0.0.".concat(String.valueOf(i))), 9001));
            outLinks.add(new InetSocketAddress(InetAddress.getByName("10.0.0.".concat(String.valueOf(i*2))), 9001));
        }
        greetingNotAcceptingCon = new P2PGreeting(9121, knownPeers);
        greetingAcceptingCon = new P2PGreeting(9121, true, outLinks, inLinks, 5, 20, knownPeers);


    }

    public void testFromJSON() throws Exception {

        P2PGreeting newGreetingNotAcceptingCon = P2PGreeting.fromJSON((JSONObject) greetingNotAcceptingCon.toJson().get("greeting"));
        assertEquals(newGreetingNotAcceptingCon, greetingNotAcceptingCon);

        P2PGreeting newGreetingAcceptingCon = P2PGreeting.fromJSON((JSONObject) greetingAcceptingCon.toJson().get("greeting"));
        assertEquals(newGreetingAcceptingCon, greetingAcceptingCon);
    }

    public void testToJson() {
        assertEquals(greetingNotAcceptingCon.toJson().toString(), notAcceptingJson);
        assertEquals(greetingAcceptingCon.toJson().toString(), acceptingJson);
    }
}