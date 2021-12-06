package org.minima.system.network.p2p.testingutils;

import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.UnknownHostException;
import java.util.*;

public class QuickInetLists {

    /**
     * Generates a list of InetSocketAddress
     * yeahprefix.num:port
     * @param prefix The first part of the ip e.g. 192.168.0. the final digit will be added the end of this
     * @param numElements Number of IP's to be generated
     * @return the list of addresses
     * @throws UnknownHostException
     */
    public static List<InetSocketAddress> generateInetSockAddrList(String prefix, int numElements, int port) throws UnknownHostException {
        List<InetSocketAddress> outLinks = new ArrayList<>();

        for (int i = 0; i < numElements; i++) {
            outLinks.add(new InetSocketAddress(InetAddress.getByName(prefix.concat(String.valueOf(i + 1))), port));
        }
        return outLinks;
    }

    /**
     * Generates a list of InetSocketAddress
     * hostname/prefix.num:port
     * @param hostname the hostname for the ip address
     * @param prefix The first part of the ip e.g. 192.168.0. the final digit will be added the end of this
     * @param numElements Number of IP's to be generated
     * @return the list of addresses
     * @throws UnknownHostException
     */
    public static List<InetSocketAddress> generateInetSockAddrWithHostnameList(String hostname, String prefix, int numElements, int port) throws UnknownHostException {
        return generateInetSockAddrList(hostname.concat("/").concat(prefix), numElements, port);
    }


    public static Map<String, InetSocketAddress> generateConnectionUIDMap(String uidPrefix, String ipPrefix, int numElements, int port) throws UnknownHostException {
        Random rand = new Random();
        Map<String, InetSocketAddress> links = new HashMap<>();
        boolean randomisePort = false;
        if (port <= 0 ) {
            randomisePort = true;
        }
        for (int i = 0; i < numElements; i++) {
            if (randomisePort) {
                port = 7000 + rand.nextInt(3000);
            }
            links.put(uidPrefix.concat(String.valueOf(i + 1)) ,new InetSocketAddress(InetAddress.getByName(ipPrefix.concat(String.valueOf(i + 1))), port));
        }
        return links;
    }
}
