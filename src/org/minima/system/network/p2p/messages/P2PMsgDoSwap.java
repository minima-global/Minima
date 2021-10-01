package org.minima.system.network.p2p.messages;

import lombok.Getter;
import lombok.Setter;
import org.minima.objects.base.MiniData;
import org.minima.utils.Streamable;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;

@Getter
@Setter
public class P2PMsgDoSwap implements Streamable {

    private MiniData secret = MiniData.getRandomData(8);
    private InetSocketAddress swapTarget;

    public P2PMsgDoSwap() {}

    @Override
    public void writeDataStream(DataOutputStream zOut) throws IOException {
        secret.writeDataStream(zOut);
        InetSocketAddressIO.writeAddress(swapTarget, zOut);
    }

    @Override
    public void readDataStream(DataInputStream zIn) throws IOException {
        setSecret(MiniData.ReadFromStream(zIn));
        setSwapTarget(InetSocketAddressIO.readAddress(zIn));
    }

    public static P2PMsgDoSwap ReadFromStream(DataInputStream zIn) throws IOException {
        P2PMsgDoSwap data = new P2PMsgDoSwap();
        data.readDataStream(zIn);
        return data;

    }
}
