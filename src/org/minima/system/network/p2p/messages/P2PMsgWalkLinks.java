package org.minima.system.network.p2p.messages;

import lombok.Getter;
import lombok.Setter;
import org.minima.objects.base.MiniData;
import org.minima.utils.Streamable;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.ArrayList;

@Setter
@Getter
public class P2PMsgWalkLinks implements Streamable {

    private MiniData secret = MiniData.getRandomData(8);
    boolean walkInLinks;
    boolean isJoiningWalk;
    boolean isReturning = false;
    int numHopsToGo = 10;
    ArrayList<InetSocketAddress> pathTaken = new ArrayList<>();


    public P2PMsgWalkLinks() {}

    public P2PMsgWalkLinks(boolean walkInLinks, boolean isJoiningWalk) {
        this.walkInLinks = walkInLinks;
        this.isJoiningWalk = isJoiningWalk;
    }

    public P2PMsgWalkLinks(boolean walkInLinks, boolean isReturning, int numHopsToGo, ArrayList<InetSocketAddress> pathTaken) {
        this.walkInLinks = walkInLinks;
        this.isReturning = isReturning;
        this.numHopsToGo = numHopsToGo;
        this.pathTaken = pathTaken;
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
        zOut.writeBoolean(walkInLinks);
        zOut.writeBoolean(isJoiningWalk);
        zOut.writeBoolean(isReturning);
        zOut.writeInt(numHopsToGo);
        InetSocketAddressIO.writeAddressList(this.pathTaken, zOut);
    }


    @Override
    public void readDataStream(DataInputStream zIn) throws IOException {
        this.setWalkInLinks(zIn.readBoolean());
        this.setJoiningWalk(zIn.readBoolean());
        this.setReturning(zIn.readBoolean());
        this.setNumHopsToGo(zIn.readInt());
        this.setPathTaken(InetSocketAddressIO.readAddressList(zIn));
    }

    public static P2PMsgWalkLinks ReadFromStream(DataInputStream zIn) throws IOException {
        P2PMsgWalkLinks msgWalkLinks = new P2PMsgWalkLinks();
        msgWalkLinks.readDataStream(zIn);
        return msgWalkLinks;
    }

}
