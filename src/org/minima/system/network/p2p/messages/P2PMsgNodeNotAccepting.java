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

/**
 * P2P message suggesting that a node is not accepting messages
 */
@AllArgsConstructor
@NoArgsConstructor(access = PRIVATE)
@Getter
public class P2PMsgNodeNotAccepting implements Streamable {
    /**
     * Node broadcasting the message
     */
    private InetSocketAddress broadcaster;

    @Override
    public void writeDataStream(DataOutputStream zOut) throws IOException {
        InetSocketAddressIO.writeAddress(broadcaster, zOut);
    }

    @Override
    public void readDataStream(DataInputStream zIn) throws IOException {
        broadcaster = InetSocketAddressIO.readAddress(zIn);
    }

    public static P2PMsgNodeNotAccepting ReadFromStream(DataInputStream zIn) throws IOException {
    P2PMsgNodeNotAccepting data = new P2PMsgNodeNotAccepting();
        data.readDataStream(zIn);
        return data;
    }
}
