package org.minima.system.network.p2p;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import lombok.Getter;
import lombok.Setter;
import org.minima.utils.MinimaLogger;

import java.net.InetAddress;
import java.util.ArrayList;

@Getter
@Setter
@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
public class P2PNode {


    private InetAddress IPAddress;
    private int basePort;
    private long lastSeenTimestamp;
    private ArrayList<P2PNode> connectedP2PNodes;
    private ArrayList<P2PNode> connectedClientNodes;
    private boolean isConnectedToNode;
    private boolean isConnectable;

    public P2PNode(){}

    public P2PNode(InetAddress IPAddress, int basePort, long lastSeenTimestamp, ArrayList<P2PNode> connectedP2PNodes, ArrayList<P2PNode> connectedClientNodes, boolean isConnectedToNode, boolean isConnectable) {
        this.IPAddress = IPAddress;
        this.basePort = basePort;
        this.lastSeenTimestamp = lastSeenTimestamp;
        this.isConnectedToNode = isConnectedToNode;
        this.isConnectable = isConnectable;
        if (connectedP2PNodes == null) {
            this.connectedP2PNodes = new ArrayList<>();
        } else {
            this.connectedP2PNodes = connectedP2PNodes;
        }

        if (connectedClientNodes == null) {
            this.connectedClientNodes = new ArrayList<>();
        } else {
            this.connectedClientNodes = connectedClientNodes;
        }
    }


    public void AddConnectedP2PNode(P2PNode node) {
        this.connectedP2PNodes.add(node);
    }

    public boolean RemoveConnectedP2PNode(P2PNode node) {
        return this.connectedP2PNodes.remove(node);
    }

    public void AddConnectedClientNode(P2PNode node) {
        this.connectedClientNodes.add(node);
    }

    public boolean RemoveConnectedClientNode(P2PNode node) {
        return this.connectedClientNodes.remove(node);
    }

    @Override
    public boolean equals(Object v) {
        boolean retVal = false;
        if (v instanceof P2PNode) {
            P2PNode ptr = (P2PNode) v;
            retVal = ptr.IPAddress == this.IPAddress;
        }
        return retVal;
    }

    @Override
    public int hashCode() {
        int hash = 7;
        hash = 17 * hash + (this.IPAddress != null ? this.IPAddress.hashCode() : 0);
        return hash;
    }

}
