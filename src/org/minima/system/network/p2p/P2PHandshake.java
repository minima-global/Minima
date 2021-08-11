package org.minima.system.network.p2p;

import lombok.Getter;
import lombok.Setter;
import org.minima.GlobalParams;

import java.net.InetAddress;
import java.util.ArrayList;


/*
 *    | 1 byte Handshake/heartbeat | 1 byte Req/Resp | 1 byte numConnectedP2PNodes n |
 *  n |
 */
@Getter
public class P2PHandshake {

    private final String version = GlobalParams.MINIMA_VERSION;
    private final P2PNode targetNode;
    private final P2PNode thisNode;
    private final ArrayList<P2PNode> knownNodesList;

    public P2PHandshake(P2PNode targetNode, P2PNode thisNode, ArrayList<P2PNode> knownNodesList) {
        this.targetNode = targetNode;
        this.thisNode = thisNode;
        this.knownNodesList = knownNodesList;
    }

}
