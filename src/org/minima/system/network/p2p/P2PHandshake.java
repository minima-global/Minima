package org.minima.system.network.p2p;

import lombok.Getter;
import lombok.Setter;

import java.net.InetAddress;
import java.util.ArrayList;

@Getter
public class P2PHandshake {

    private final P2PNode targetNode;
    private final P2PNode thisNode;
    private final ArrayList<P2PNode> knownNodesList;

    public P2PHandshake(P2PNode targetNode, P2PNode thisNode, ArrayList<P2PNode> knownNodesList) {
        this.targetNode = targetNode;
        this.thisNode = thisNode;
        this.knownNodesList = knownNodesList;
    }

}
