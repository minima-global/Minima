package org.minima.system.network.p2p;

import org.junit.Before;
import org.junit.Test;

import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.util.ArrayList;

import static org.junit.Assert.*;

public class P2PDBTest {

    ArrayList<InetSocketAddress> testPeers;

    @Before
    public void before() throws Exception {

        testPeers = new ArrayList<>();

        for(int ii = 1; ii < 5; ii++) {
            for (int i = 0; i < 250; i++) {
                testPeers.add(new InetSocketAddress(InetAddress.getByName("192.168.".concat(String.valueOf(ii)).concat(".").concat(String.valueOf(i))), 9001));
            }
        }

    }


    @Test
    public void testGetEmptyPeersList() {

        P2PDB db = new P2PDB();

        ArrayList<InetSocketAddress> emptyPeers = db.getPeersList();
        // Check loading from an empty database
        // returns an empty lits
        assertTrue(emptyPeers.isEmpty());


    }

    @Test
    public void testSetPeersList() {

        P2PDB db = new P2PDB();

        db.setPeersList(testPeers);

    }

    @Test
    public void testSetAndGetPeersList() {

        P2PDB db = new P2PDB();

        db.setPeersList(testPeers);

        ArrayList<InetSocketAddress> loadedPeers = db.getPeersList();

        assertFalse(loadedPeers.isEmpty());
        assertEquals(loadedPeers.size(), 1000);

    }

}
