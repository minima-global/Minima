package org.minima.system.network.p2p.messages;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.minima.objects.base.MiniData;
import org.minima.system.network.p2p.Traceable;
import org.minima.system.network.p2p.event.EventPublisher;
import org.minima.system.network.p2p.util.JSONObjectUtils;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import static java.util.stream.Collectors.toList;
import static lombok.AccessLevel.PRIVATE;

@NoArgsConstructor(access = PRIVATE)
@Setter
@Getter
public class P2PMsgWalkLinks implements Streamable, Traceable {

    private MiniData traceId;
    private MiniData secret = MiniData.getRandomData(8);
    boolean walkInLinks;
    boolean isJoiningWalk;
    boolean isClientWalk;
    int availableClientSlots = 0;
    boolean isReturning = false;
    int numHopsToGo = 10;
    ArrayList<InetSocketAddress> pathTaken = new ArrayList<>();


    public P2PMsgWalkLinks(boolean walkInLinks, boolean isJoiningWalk, Traceable traceable) {
        this.walkInLinks = walkInLinks;
        this.isJoiningWalk = isJoiningWalk;
        traceId = new MiniData(traceable.getTraceId());
    }

    public void addHopToPath(InetSocketAddress address) {
        assert !isReturning: "Attempting to add hop to path whilst returning";
        this.pathTaken.add(address);
        numHopsToGo--;
        assert this.pathTaken.size() + numHopsToGo == 10: "Combined hops and path != 10 -  numHopsToGo: " + numHopsToGo + " PathTaken.size(): " + this.pathTaken.size();
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
        traceId.writeDataStream(zOut);
        secret.writeDataStream(zOut);
        zOut.writeBoolean(walkInLinks);
        zOut.writeBoolean(isJoiningWalk);
        zOut.writeBoolean(isClientWalk);
        zOut.writeInt(availableClientSlots);
        zOut.writeBoolean(isReturning);
        zOut.writeInt(numHopsToGo);
        InetSocketAddressIO.writeAddressList(this.pathTaken, zOut);
        EventPublisher.publishWrittenStream(this);
    }


    @Override
    public void readDataStream(DataInputStream zIn) throws IOException {
        this.setTraceId(MiniData.ReadFromStream(zIn));
        this.setSecret(MiniData.ReadFromStream(zIn));
        this.setWalkInLinks(zIn.readBoolean());
        this.setJoiningWalk(zIn.readBoolean());
        this.setClientWalk(zIn.readBoolean());
        this.setAvailableClientSlots(zIn.readInt());
        this.setReturning(zIn.readBoolean());
        this.setNumHopsToGo(zIn.readInt());
        this.setPathTaken(InetSocketAddressIO.readAddressList(zIn));
        EventPublisher.publishReadStream(this);
    }

    public static P2PMsgWalkLinks ReadFromStream(DataInputStream zIn) throws IOException {
        P2PMsgWalkLinks msgWalkLinks = new P2PMsgWalkLinks();
        msgWalkLinks.readDataStream(zIn);
        return msgWalkLinks;
    }

    @Override
    public String getTraceId() {
        return traceId.toString();
    }

    @Override
    public JSONObject getContent() {

        Map<String, Object> map = new HashMap<>();
        map.put("walkInLinks", walkInLinks);
        map.put("isJoiningWalk", isJoiningWalk);
        map.put("isClientWalk", isClientWalk);
        map.put("availableClientSlots", availableClientSlots);
        map.put("isReturning", isReturning);
        map.put("numHopsToGo", numHopsToGo);
        map.put("pathTaken", new JSONArray(pathTaken.stream()
                .map(JSONObjectUtils::from)
                .collect(toList())));
        return new JSONObject(map);
    }
}
