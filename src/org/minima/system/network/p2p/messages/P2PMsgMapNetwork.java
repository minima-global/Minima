package org.minima.system.network.p2p.messages;

import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.minima.utils.Streamable;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.ArrayList;

@Getter
@Setter
@Slf4j
public class P2PMsgMapNetwork implements Streamable {
    private ArrayList<InetSocketAddress> addresses;
    private int numClients;

    public P2PMsgMapNetwork() {}

    public P2PMsgMapNetwork(ArrayList<InetSocketAddress> addresses, int numClients) {
        this.addresses = addresses;
        this.numClients = numClients;
    }

    public static P2PMsgMapNetwork ReadFromStream(DataInputStream zIn) throws IOException {
        P2PMsgMapNetwork rendezvous = new P2PMsgMapNetwork();
        rendezvous.readDataStream(zIn);
        return rendezvous;
    }

    @Override
    public void writeDataStream(DataOutputStream zOut) throws IOException {
        InetSocketAddressIO.writeAddressList(this.addresses, zOut);
        zOut.writeInt(numClients);
    }

    @Override
    public void readDataStream(DataInputStream zIn) throws IOException {
        this.setAddresses(InetSocketAddressIO.readAddressList(zIn));
        this.setNumClients(zIn.readInt());
    }
}
