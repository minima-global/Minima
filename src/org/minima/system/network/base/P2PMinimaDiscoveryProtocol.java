package org.minima.system.network.base;


import io.libp2p.core.PeerId;
import io.libp2p.core.Stream;
//import io.libp2p.etc.types.toByteBuf;
import io.libp2p.protocol.ProtocolHandler;
import io.libp2p.protocol.ProtocolMessageHandler;
import io.netty.buffer.ByteBuf;

import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.util.concurrent.CompletableFuture;

interface P2PMinimaDiscoveryProtocolController {
    //fun send(message: String)
    public void send(String message);
}

// class Ping : PingBinding(PingProtocol())

// open class PingBinding(ping: PingProtocol) :
//     StrictProtocolBinding<PingController>("/ipfs/ping/1.0.0", ping)

public class P2PMinimaDiscoveryProtocol extends ProtocolHandler<P2PMinimaDiscoveryProtocolController> {

    public P2PMinimaDiscoveryProtocol() {
        super(Long.MAX_VALUE, Long.MAX_VALUE);
        //TODO Auto-generated constructor stub
    }
   
    private  CompletableFuture<P2PMinimaDiscoveryProtocolController> onStart(Stream stream) {
        // TODO: add a handler and do something
        CompletableFuture<P2PMinimaDiscoveryProtocolController> ready = new CompletableFuture<P2PMinimaDiscoveryProtocolController>();
        // val handler = MDPHandler(chatCallback, ready)
        //stream.pushHandler(handler)
        //return ready.thenApply { handler }
        return ready;
    }

    protected CompletableFuture<P2PMinimaDiscoveryProtocolController> onStartInitiator(Stream stream) {
        return onStart(stream);
    }

    protected CompletableFuture<P2PMinimaDiscoveryProtocolController> onStartResponder(Stream stream) {
        return onStart(stream);
    }

    class MDPHandler implements ProtocolMessageHandler<ByteBuf>, P2PMinimaDiscoveryProtocolController {

        private Stream stream;
       // private mdpCallback;

        public MDPHandler() {
            //todo: add OnChatMessage and ready equivalents in constructor and save as private fields
        }

        @Override
        public void onActivated(Stream stream) {
            this.stream = stream;
            //ready.complete(null);
        }

        @Override
        public void send(String message) {
            byte[] data = message.getBytes(Charset.defaultCharset());
            stream.writeAndFlush(data); // does this need to be in a ByteBuffer
        }

        @Override
        public void fireMessage(Stream arg0, Object arg1) {
            // TODO Auto-generated method stub
            
        }

        @Override
        public void onClosed(Stream arg0) {
            // TODO Auto-generated method stub
            
        }

        @Override
        public void onException(Throwable arg0) {
            // TODO Auto-generated method stub
            
        }

        @Override
        public void onMessage(Stream stream, ByteBuf msg) {
            String msgStr = msg.toString(Charset.defaultCharset());
            System.out.println("Received message: " + msgStr);
            //mdpCallback(stream.remotePeerId(), msgStr);            
        }
    }

//     open inner class Chatter(
//         private val chatCallback: OnChatMessage,
//         val ready: CompletableFuture<Void>
// ) : ProtocolMessageHandler<ByteBuf>, ChatController {
//     lateinit var stream: Stream

//     override fun onActivated(stream: Stream) {
//         this.stream = stream
//         ready.complete(null)
//     }

//     override fun onMessage(stream: Stream, msg: ByteBuf) {
//         val msgStr = msg.toString(Charset.defaultCharset())
//         chatCallback(stream.remotePeerId(), msgStr)
//     }

//     override fun send(message: String) {
//         val data = message.toByteArray(Charset.defaultCharset())
//         stream.writeAndFlush(data.toByteBuf())
//     }
// }

}
