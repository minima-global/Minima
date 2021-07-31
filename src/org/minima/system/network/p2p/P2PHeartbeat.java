package org.minima.system.network.p2p;

import lombok.Getter;
import lombok.Setter;

import java.net.InetAddress;

@Getter
public class P2PHeartbeat {
    private final P2PNode targetNode;
    private final P2PNode thisNode;

    public P2PHeartbeat(P2PNode targetNode, P2PNode thisNode) {
        this.targetNode = targetNode;
        this.thisNode = thisNode;
    }

}
