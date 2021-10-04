package org.minima.system.network.p2p.messages;

import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.ArrayList;

@Getter
@Setter
@Slf4j
public class P2PMsgNetworkMap implements Streamable {
    InetSocketAddress nodeAddress;
    private ArrayList<InetSocketAddress> addresses;
    private int numClients;

    public P2PMsgNetworkMap() {}

    public P2PMsgNetworkMap(InetSocketAddress nodeAddress, ArrayList<InetSocketAddress> addresses, int numClients) {
        this.nodeAddress = nodeAddress;
        this.addresses = addresses;
        this.numClients = numClients;
    }

    public static P2PMsgNetworkMap ReadFromStream(DataInputStream zIn) throws IOException {
        P2PMsgNetworkMap rendezvous = new P2PMsgNetworkMap();
        rendezvous.readDataStream(zIn);
        return rendezvous;
    }

    @Override
    public void writeDataStream(DataOutputStream zOut) throws IOException {
        InetSocketAddressIO.writeAddress(nodeAddress, zOut);
        InetSocketAddressIO.writeAddressList(this.addresses, zOut);
        zOut.writeInt(numClients);
    }

    @Override
    public void readDataStream(DataInputStream zIn) throws IOException {
        this.setNodeAddress(InetSocketAddressIO.readAddress(zIn));
        this.setAddresses(InetSocketAddressIO.readAddressList(zIn));
        this.setNumClients(zIn.readInt());
    }


    public JSONObject toNodeJSON(){
        JSONObject json = new JSONObject();
        json.put("id", nodeAddress.toString());
        json.put("num_clients", numClients);
        return json;
    }

    public JSONArray toLinksJSON(){
        JSONArray links = new JSONArray();
        for (InetSocketAddress inetSocketAddress: addresses){
            JSONObject link = new JSONObject();
            link.put("source", nodeAddress.toString());
            link.put("target", inetSocketAddress.toString());
            links.add(link);
        }
        return links;
    }
}
