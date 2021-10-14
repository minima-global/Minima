package org.minima.system.network.p2p.messages;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.minima.utils.Streamable;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.InetSocketAddress;

import static lombok.AccessLevel.PRIVATE;

@AllArgsConstructor
@NoArgsConstructor(access = PRIVATE)
@Getter
public class P2PMsgIsClient implements Streamable {

    private boolean isClient;
    private InetSocketAddress broadcaster;

    @Override
    public void writeDataStream(DataOutputStream zOut) throws IOException {
        zOut.writeBoolean(isClient);
        InetSocketAddressIO.writeAddress(broadcaster, zOut);
    }

    @Override
    public void readDataStream(DataInputStream zIn) throws IOException {
        isClient = zIn.readBoolean();
        broadcaster = InetSocketAddressIO.readAddress(zIn);
    }

    public static P2PMsgIsClient ReadFromStream(DataInputStream zIn) throws IOException {
        P2PMsgIsClient data = new P2PMsgIsClient();
        data.readDataStream(zIn);
        return data;
    }
}
