package org.minima.system.network.p2p;

import junit.framework.TestCase;
import org.minima.system.network.minima.NIOClientInfo;
import org.minima.system.network.p2p.testingutils.QuickClients;
import org.minima.system.network.p2p.testingutils.QuickInetLists;

import java.net.InetSocketAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.List;

public class UtilFuncsTest extends TestCase {

//    public void testGetClientFromInetAddressEitherDirection() {
//        List<NIOClientInfo> clients = QuickClients.generateClientInfoList("10.0.0.", 5, 9001, "inUid", true);
//        clients.addAll(QuickClients.generateClientInfoList("20.0.0.", 5, 9001, "inUid", false));
//        InetSocketAddress inAddress = new InetSocketAddress("10.0.0.2", 9001);
//        NIOClientInfo inClient = UtilFuncs.getClientFromInetAddressEitherDirection(inAddress, clients);
//        InetSocketAddress outAddress = new InetSocketAddress("20.0.0.2", 9001);
//        NIOClientInfo outClient = UtilFuncs.getClientFromInetAddressEitherDirection(outAddress, clients);
//        InetSocketAddress nullAddress = new InetSocketAddress("30.0.0.2", 9001);
//        NIOClientInfo nullClient = UtilFuncs.getClientFromInetAddressEitherDirection(nullAddress, clients);
//
//        assertEquals("10.0.0.2", inClient.getHost());
//        assertEquals("20.0.0.2", outClient.getHost());
//        assertNull(nullClient);
//    }

    public void testSelectRandomAddress() throws UnknownHostException {
        List<InetSocketAddress> addresses = QuickInetLists.generateInetSockAddrList("192.168.0.", 10, 9001);
        InetSocketAddress address = UtilFuncs.selectRandomAddress(addresses);
        assertNotNull(address);
    }

    public void testSelectRandomAddressEmptyList() {
        ArrayList<InetSocketAddress> addresses = new ArrayList<>();
        InetSocketAddress address = UtilFuncs.selectRandomAddress(addresses);
        assertNull(address);
    }
}