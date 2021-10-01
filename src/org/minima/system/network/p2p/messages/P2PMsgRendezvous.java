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


    @Override
    public void writeDataStream(DataOutputStream zOut) throws IOException {
        InetSocketAddressIO.writeAddressList(this.addresses, zOut);
    }


    @Override
    public void readDataStream(DataInputStream zIn) throws IOException {
        this.setAddresses(InetSocketAddressIO.readAddressList(zIn));
    }

    public static P2PMsgRendezvous ReadFromStream(DataInputStream zIn) throws IOException {
        P2PMsgRendezvous rendezvous = new P2PMsgRendezvous(null);
        rendezvous.readDataStream(zIn);
        return rendezvous;
    }
}
