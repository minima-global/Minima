package org.minima.system.network.p2p.messages;

import lombok.Getter;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.util.ArrayList;

@Getter
public class P2PNode implements Streamable {

    InetSocketAddress address;
    ArrayList<InetSocketAddress> links = new ArrayList<>();
    int numClients;

    public P2PNode() {}

    public P2PNode(InetSocketAddress address, ArrayList<InetSocketAddress> links, int numClients) {
        this.address = address;
        this.links = links;
        this.numClients = numClients;
    }

    @Override
    public void writeDataStream(DataOutputStream zOut) throws IOException {
        // Address
        InetSocketAddressIO.writeAddress(address, zOut);
        // num Clients
        zOut.writeInt(numClients);
        // Links
        InetSocketAddressIO.writeAddressList(this.links, zOut);

    }

    @Override
    public void readDataStream(DataInputStream zIn) throws IOException {
        // Address
        this.address = InetSocketAddressIO.readAddress(zIn);
        this.numClients = zIn.readInt();
        this.links = InetSocketAddressIO.readAddressList(zIn);


    }

    public static P2PNode ReadFromStream(DataInputStream zIn) throws IOException {
        P2PNode data = new P2PNode();
        data.readDataStream(zIn);
        return data;

    }

    public JSONArray toJSONArray() {
        JSONArray nodes = new JSONArray();
        nodes.add(address.toString());
        for(InetSocketAddress link: links){
            nodes.add(link.toString());
        }
        return nodes;
    }
}
