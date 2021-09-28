package org.minima.system.network.p2p.messages;

import org.minima.utils.Streamable;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;


public class P2PMsgMapNetwork implements Streamable {

    public P2PMsgMapNetwork() {}

    @Override
    public void writeDataStream(DataOutputStream zOut) throws IOException {
    }

    @Override
    public void readDataStream(DataInputStream zIn) throws IOException {
    }

    public static P2PMsgMapNetwork ReadFromStream(DataInputStream zIn) throws IOException {
        P2PMsgMapNetwork data = new P2PMsgMapNetwork();
        data.readDataStream(zIn);
        return data;

    }
}
