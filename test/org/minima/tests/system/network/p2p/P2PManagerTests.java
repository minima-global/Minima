package org.minima.tests.system.network.p2p;
import org.junit.Test;
import org.minima.Start;
import org.minima.system.network.p2p.P2PManager;

import java.io.File;
import java.net.InetAddress;

import static org.junit.Assert.*;

public class P2PManagerTests {

    @Test
    public void testInitLoadDefaults() throws Exception {
        InetAddress ip = InetAddress.getLocalHost();
        File dataFile = new File("p2pdatafile.json");
        P2PManager p2p = new P2PManager(ip, 9001, dataFile);

        assertEquals(p2p.GetActiveNodeList().size(), Start.VALID_BOOTSTRAP_NODES.length);
    }

    @Test
    public void testInitPassList() throws Exception {
        InetAddress ip = InetAddress.getLocalHost();
        File dataFile = new File("p2pdatafile.json");
        P2PManager p2p = new P2PManager(ip, 9001, dataFile);

        assertEquals(p2p.GetActiveNodeList().size(), Start.VALID_BOOTSTRAP_NODES.length);
    }

}
