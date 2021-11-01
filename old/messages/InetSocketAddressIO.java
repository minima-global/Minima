package org.minima.system.network.p2p.old.messages;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.util.ArrayList;

public class InetSocketAddressIO {

    static public void writeAddress(InetSocketAddress address, DataOutputStream zOut) throws IOException {
        zOut.writeInt(address.getAddress().getAddress().length);
        zOut.write(address.getAddress().getAddress());
        zOut.writeInt(address.getPort());
    }


    static public void writeAddressList(ArrayList<InetSocketAddress> nodes, DataOutputStream zOut) throws IOException {
        zOut.writeInt(nodes.size());
        for (InetSocketAddress address : nodes) {
            writeAddress(address, zOut);
        }
    }

    static public InetSocketAddress readAddress(DataInputStream zIn) throws IOException {
        int nodeAddrLen = zIn.readInt();
        byte[] nodeAddr = new byte[nodeAddrLen];
        zIn.readFully(nodeAddr);
        int nodePort = zIn.readInt();
        return new InetSocketAddress(InetAddress.getByAddress(nodeAddr), nodePort);
    }

    static public ArrayList<InetSocketAddress> readAddressList(DataInputStream zIn) throws IOException {
        ArrayList<InetSocketAddress> addressArrayList = new ArrayList<>();
        int numPeers = zIn.readInt();
        for (int i = 0; i < numPeers; i++) {
            InetSocketAddress nodeAddress = readAddress(zIn);
            addressArrayList.add(nodeAddress);
        }

        return addressArrayList;
    }

}
