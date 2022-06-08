package org.minima.system.network.p2p.messages;

import junit.framework.TestCase;

import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.util.ArrayList;

public class InetSocketAddressIOTest extends TestCase {

    ArrayList<InetSocketAddress> peers;
    static final String peersJson = "[{\"host\":\"192.168.0.1\",\"port\":9001},{\"host\":\"192.168.0.2\",\"port\":9001}]";

    public void setUp() throws Exception {
        super.setUp();

        peers = new ArrayList<>();

        for (int i = 1; i < 3; i++) {
            peers.add(new InetSocketAddress(InetAddress.getByName("192.168.0.".concat(String.valueOf(i))), 9001));
        }
    }

    public void testAddressesListToJSON() {
        assertEquals(InetSocketAddressIO.addressesListToJSON(peers).toJSONString(), peersJson);
    }

    public void testAddressesJSONToList() {
        assertEquals(InetSocketAddressIO.addressesJSONToList(InetSocketAddressIO.addressesListToJSON(peers)), peers);
    }
}