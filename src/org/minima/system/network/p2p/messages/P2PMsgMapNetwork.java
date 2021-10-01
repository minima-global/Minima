package org.minima.system.network.p2p.messages;

import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.minima.objects.base.MiniData;
import org.minima.system.network.p2p.P2PState;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Getter
@Setter
@Slf4j
public class P2PMsgMapNetwork implements Streamable {

    private MiniData requestUID = MiniData.getRandomData(8);
    private MiniData msgUID = MiniData.getRandomData(8);

    Map<InetSocketAddress, P2PNode> nodes = new HashMap<>();
    ArrayList<InetSocketAddress> pathTaken = new ArrayList<>();

    public P2PMsgMapNetwork() {
    }

    public P2PMsgMapNetwork(P2PMsgMapNetwork old) {
        this.requestUID = old.getRequestUID();
        this.msgUID = old.getMsgUID();
        this.nodes = old.getNodes();
        this.pathTaken = old.getPathTaken();
    }

    public P2PMsgMapNetwork(Map<InetSocketAddress, P2PNode> pNodeMap) {
        this.nodes = pNodeMap;
    }

    public P2PMsgMapNetwork(ArrayList<P2PMsgMapNetwork> multipleMapMessages, ArrayList<InetSocketAddress> path, MiniData requestUID) {
        this.pathTaken = path;
        this.requestUID = requestUID;
        log.debug("[=] P2PMsgMapNetwork multipleMapMessages size: " + multipleMapMessages.size());
        for (P2PMsgMapNetwork p2PMsgMapNetwork : multipleMapMessages) {
            log.debug("[=] P2PMsgMapNetwork multipleMapMessages.item size: " + p2PMsgMapNetwork.getNodes().size());
            for (InetSocketAddress key : p2PMsgMapNetwork.getNodes().keySet()) {
                P2PNode tmp = p2PMsgMapNetwork.getNodes().get(key);
                log.debug("[=] P2PMsgMapNetwork multipleMapMessages.item.link address: "+ tmp.getAddress() + " num links: " + tmp.getLinks().size());
                // We don't care which list the node details have come from
                nodes.put(key, tmp);
            }
        }
    }

    public void addThisNodeFromState(P2PState state){
        ArrayList<InetSocketAddress> uniqueLinks = Stream.of(state.getInLinks(), state.getOutLinks())
                .flatMap(Collection::stream).distinct().collect(Collectors.toCollection(ArrayList::new));
        P2PNode node = new P2PNode(state.getAddress(), uniqueLinks, state.getClientLinks().size());
        this.nodes.put(state.getAddress(), node);
    }


    public InetSocketAddress getPreviousNode(InetSocketAddress thisAddress) {
        InetSocketAddress previousNode = null;
        assert this.pathTaken.contains(thisAddress) : "Node " + thisAddress + " not in path: " + this.pathTaken;
        int index = this.pathTaken.indexOf(thisAddress);
        if (index > 0) {
            index--;
            previousNode = this.pathTaken.get(index);
        }
        return previousNode;
    }

    @Override
    public void writeDataStream(DataOutputStream zOut) throws IOException {
        requestUID.writeDataStream(zOut);
        msgUID.writeDataStream(zOut);
        zOut.writeInt(nodes.size());
        for(P2PNode node: nodes.values()){
            node.writeDataStream(zOut);
        }
        InetSocketAddressIO.writeAddressList(pathTaken, zOut);

    }

    @Override
    public void readDataStream(DataInputStream zIn) throws IOException {
        requestUID = MiniData.ReadFromStream(zIn);
        msgUID = MiniData.ReadFromStream(zIn);
        int nodeCount = zIn.readInt();
        for(int i=0; i< nodeCount; i++){
            P2PNode tmp = P2PNode.ReadFromStream(zIn);
            nodes.put(tmp.address, tmp);
        }
        pathTaken = InetSocketAddressIO.readAddressList(zIn);
    }

    public static P2PMsgMapNetwork ReadFromStream(DataInputStream zIn) throws IOException {
        P2PMsgMapNetwork data = new P2PMsgMapNetwork();
        data.readDataStream(zIn);
        return data;

    }

    public JSONArray toJSONArray() {
        JSONArray links = new JSONArray();
        for(P2PNode link: nodes.values()){
            links.add(link.toJSONArray());
        }
        return links;
    }
}
