package org.minima.system.network.p2p.messages;

import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.minima.system.network.p2p.P2PState;
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
public class P2PMsgNode implements Streamable {
    InetSocketAddress nodeAddress;
    private ArrayList<InetSocketAddress> inLinks;
    private ArrayList<InetSocketAddress> outLinks;
    private ArrayList<InetSocketAddress> clientLinks;

    public P2PMsgNode() {
    }

    public P2PMsgNode(P2PState state) {
        this.nodeAddress = state.getAddress();
        this.inLinks = state.getInLinks();
        this.outLinks = state.getOutLinks();
        this.clientLinks = state.getClientLinks();
    }

    public static P2PMsgNode ReadFromStream(DataInputStream zIn) throws IOException {
        P2PMsgNode rendezvous = new P2PMsgNode();
        rendezvous.readDataStream(zIn);
        return rendezvous;
    }

    @Override
    public void writeDataStream(DataOutputStream zOut) throws IOException {
        InetSocketAddressIO.writeAddress(nodeAddress, zOut);
        InetSocketAddressIO.writeAddressList(this.inLinks, zOut);
        InetSocketAddressIO.writeAddressList(this.outLinks, zOut);
        InetSocketAddressIO.writeAddressList(this.clientLinks, zOut);
    }

    @Override
    public void readDataStream(DataInputStream zIn) throws IOException {
        this.setNodeAddress(InetSocketAddressIO.readAddress(zIn));
        this.setInLinks(InetSocketAddressIO.readAddressList(zIn));
        this.setOutLinks(InetSocketAddressIO.readAddressList(zIn));
        this.setClientLinks(InetSocketAddressIO.readAddressList(zIn));
    }

    public JSONObject toNodeJSON(){
        JSONObject json = new JSONObject();
        json.put("id", nodeAddress.toString());
        json.put("num_clients", getClientLinks().size());
        return json;
    }

    public JSONArray toLinksJSON(){
        JSONArray links = new JSONArray();
        for (InetSocketAddress inetSocketAddress: getOutLinks()){
            JSONObject link = new JSONObject();
            link.put("source", nodeAddress.toString());
            link.put("target", inetSocketAddress.toString());
            links.add(link);
        }
        return links;
    }

    public JSONObject toDetailsJSON(){
        JSONObject json = new JSONObject();
        json.put("address", nodeAddress.toString().replaceAll("/",""));
        json.put("out_links", addressListToJSONArray(outLinks));
        json.put("in_links", addressListToJSONArray(inLinks));
        json.put("client_links", addressListToJSONArray(clientLinks));

        return json;
    }

    private JSONArray addressListToJSONArray(ArrayList<InetSocketAddress> addresses){
        JSONArray links = new JSONArray();
        for (InetSocketAddress inetSocketAddress: addresses){
            links.add(inetSocketAddress.toString().replaceAll("/",""));
        }
        return links;
    }
}
