package org.minima.system.network.p2p.messages;

import lombok.Getter;
import lombok.Setter;
import org.minima.utils.Streamable;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.util.ArrayList;

@Getter
@Setter
public class P2PMsgRendezvous implements Streamable {
    private ArrayList<InetSocketAddress> addresses;

    public P2PMsgRendezvous(ArrayList<InetSocketAddress> addresses) {
        this.addresses = addresses;
    }


    static public void writeNodeArrayList(ArrayList<InetSocketAddress> nodes, DataOutputStream zOut) throws IOException {
        zOut.writeInt(nodes.size());
        for (InetSocketAddress address : nodes) {
            zOut.writeInt(address.getAddress().getAddress().length);
            zOut.write(address.getAddress().getAddress());
            zOut.writeInt(address.getPort());
        }
    }

    @Override
    public void writeDataStream(DataOutputStream zOut) throws IOException {
        writeNodeArrayList(this.addresses, zOut);
    }

    static public ArrayList<InetSocketAddress> readNodeArrayList(DataInputStream zIn) throws IOException {
        ArrayList<InetSocketAddress> addressArrayList = new ArrayList<>();
        int numPeers = zIn.readInt();
        for (int i = 0; i < numPeers; i++) {
            int nodeAddrLen = zIn.readInt();
            byte[] nodeAddr = new byte[nodeAddrLen];
            zIn.readFully(nodeAddr);
            int nodePort = zIn.readInt();
            InetSocketAddress nodeAddress = new InetSocketAddress(InetAddress.getByAddress(nodeAddr), nodePort);
            addressArrayList.add(nodeAddress);
        }

        return addressArrayList;
    }

    @Override
    public void readDataStream(DataInputStream zIn) throws IOException {
        this.setAddresses(readNodeArrayList(zIn));
    }

    public static P2PMsgRendezvous ReadFromStream(DataInputStream zIn) throws IOException {
        P2PMsgRendezvous rendezvous = new P2PMsgRendezvous(null);
        rendezvous.readDataStream(zIn);
        return rendezvous;
    }
}
