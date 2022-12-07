package org.minima.system.network.p2p.messages;


import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.util.ArrayList;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class InetSocketAddressIOTest {

    ArrayList<InetSocketAddress> peers;
    static final String peersJson = "[{\"host\":\"192.168.0.1\",\"port\":9001},{\"host\":\"192.168.0.2\",\"port\":9001}]";

    @BeforeEach
    public void setUp() throws Exception {
        peers = new ArrayList<>();

        for (int i = 1; i < 3; i++) {
            peers.add(new InetSocketAddress(InetAddress.getByName("192.168.0.".concat(String.valueOf(i))), 9001));
        }
    }

    @Test
    public void testAddressesListToJSON() {
        assertEquals(InetSocketAddressIO.addressesListToJSON(peers).toJSONString(), peersJson);
    }

    @Test
    public void testAddressesJSONToList() {
        assertEquals(InetSocketAddressIO.addressesJSONToList(InetSocketAddressIO.addressesListToJSON(peers)), peers);
    }
}