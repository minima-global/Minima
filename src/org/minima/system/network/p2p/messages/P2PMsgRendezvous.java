package org.minima.system.network.p2p.messages;

import lombok.Getter;
import lombok.Setter;
import org.minima.objects.base.MiniData;
import org.minima.system.network.p2p.Traceable;
import org.minima.system.network.p2p.event.EventPublisher;
import org.minima.utils.Streamable;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.ArrayList;

@Getter
@Setter
public class P2PMsgRendezvous implements Streamable, Traceable {
    private MiniData secret = MiniData.getRandomData(8);
    private ArrayList<InetSocketAddress> addresses;
    InetSocketAddress targetAddress;


    public P2PMsgRendezvous(ArrayList<InetSocketAddress> addresses, InetSocketAddress targetAddress) {
        this.addresses = addresses;
        this.targetAddress = targetAddress;
    }

    @Override
    public void writeDataStream(DataOutputStream zOut) throws IOException {
        InetSocketAddressIO.writeAddress(targetAddress, zOut);
        InetSocketAddressIO.writeAddressList(this.addresses, zOut);
        EventPublisher.publishWrittenStream(this);
    }

    @Override
    public void readDataStream(DataInputStream zIn) throws IOException {
        this.setTargetAddress(InetSocketAddressIO.readAddress(zIn));
        this.setAddresses(InetSocketAddressIO.readAddressList(zIn));
        EventPublisher.publishReadStream(this);
    }

    public static P2PMsgRendezvous ReadFromStream(DataInputStream zIn) throws IOException {
        P2PMsgRendezvous rendezvous = new P2PMsgRendezvous(null, null);
        rendezvous.readDataStream(zIn);
        return rendezvous;
    }

    @Override
    public String getTraceId() {
        return secret.to0xString();
    }
}
