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
import org.apache.logging.log4j.LogManager;

public class P2PStart {


    private static final Logger logger = LogManager.getLogger(P2PStart.class);

    public static void main(String[] args) 
             throws ExecutionException, InterruptedException {
        System.out.println("Hello world!");
       

        // Create a libp2p node and configure it
        // to accept TCP connections on a random port (0)
        // Host node = new HostBuilder()
        //   .protocol(new Ping())
        //   .listen("/ip4/127.0.0.1/tcp/0")
        //   .build();

        Host node = new HostBuilder()
          .protocol(new P2PMinimaDiscovery())
          .listen("/ip4/127.0.0.1/tcp/0")
          .build();

        // start listening
        node.start().get();

    System.out.print("Node started and listening for P2P Minima Discovery Protocol on ");
    System.out.println(node.listenAddresses());

    if (args.length > 0) {
      System.out.println("Found args: " + args[0]);
      Multiaddr address = Multiaddr.fromString(args[0]);
      P2PMinimaDiscovery disc = new P2PMinimaDiscovery();
      System.out.println("Created disc, looking up controller...");
      P2PMinimaDiscoveryProtocolController discoverer = disc.dial(
        node,
        address
      ).getController().get();

      // P2PMinimaDiscoveryProtocolController discoverer = new P2PMinimaDiscovery().dial(
      //   node,
      //   address
      // ).getController().get();

      System.out.println("Sending a message to the world...");
      discoverer.send("Hello world!");

      node.stop().get();
      // System.out.println("Sending 5 ping messages to " + address.toString());
      // for (int i = 1; i <= 5; ++i) {
      //   long latency = pinger.ping().get();
      //   System.out.println("Ping " + i + ", latency " + latency + "ms");
      // }

      // node.stop().get();
    }
  }

}

