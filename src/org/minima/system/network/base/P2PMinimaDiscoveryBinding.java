package org.minima.system.network.base;

import io.libp2p.core.multistream.StrictProtocolBinding;

public class P2PMinimaDiscoveryBinding extends StrictProtocolBinding<P2PMinimaDiscoveryProtocolController> {
    public P2PMinimaDiscoveryBinding(P2PMinimaDiscoveryProtocol mdp) {
        super("/p2p/minima/1.0.0", mdp);
    }
}