package org.minima.system.network.base;

import io.libp2p.core.Host;
import io.libp2p.core.PeerId;
import io.libp2p.core.crypto.PrivKey;
import io.libp2p.core.dsl.Builder.Defaults;
import io.libp2p.core.dsl.BuilderJKt;
import io.libp2p.core.multiformats.Multiaddr;
import io.libp2p.core.multistream.ProtocolBinding;
import io.libp2p.core.mux.StreamMuxerProtocol;
import io.libp2p.etc.types.ByteArrayExtKt;
import io.libp2p.protocol.Identify;
import io.libp2p.protocol.Ping;
import io.libp2p.security.noise.NoiseXXSecureChannel;
import io.libp2p.transport.tcp.TcpTransport;

import io.libp2p.core.Host;
import io.libp2p.core.dsl.HostBuilder;
import io.libp2p.core.multiformats.Multiaddr;
import io.libp2p.protocol.Ping;
import io.libp2p.protocol.PingController;

import java.util.concurrent.ExecutionException;

// Import log4j classes.
import org.apache.logging.log4j.Logger;
import org.minima.system.network.base.libp2p.gossip.LibP2PGossipNetwork;
import org.apache.logging.log4j.LogManager;

public class P2PStart {


    private static final Logger logger = LogManager.getLogger(P2PStart.class);

    public static void main(String[] args) 
             throws ExecutionException, InterruptedException {
        System.out.println("Hello world!");
     //  LibP2PGossipNetwork gossipNet = new LibP2PGossipNetwork(new MetricsSystem());
        
    }

}

