//package org.minima.system.network.p2p.messages;
//
//import lombok.Getter;
//import lombok.Setter;
//import org.minima.objects.base.MiniData;
//import org.minima.utils.Streamable;
//
//import java.io.DataInputStream;
//import java.io.DataOutputStream;
//import java.io.IOException;
//import java.net.InetSocketAddress;
//
//@Getter
//@Setter
//public class P2PSwapLink implements Streamable {
//
//    private MiniData secret = MiniData.getRandomData(8);
//    private InetSocketAddress swapTarget;
//    private boolean isSwapClientReq = false;
//    private boolean isConditionalSwapReq = false;
//
//    public P2PSwapLink(){}
//
//    @Override
//    public void writeDataStream(DataOutputStream zOut) throws IOException {
//        secret.writeDataStream(zOut);
//        InetSocketAddressIO.writeAddress(swapTarget, zOut);
//    }
//
//    @Override
//    public void readDataStream(DataInputStream zIn) throws IOException {
//        setSecret(MiniData.ReadFromStream(zIn));
//        setSwapTarget(InetSocketAddressIO.readAddress(zIn));
//    }
//
//    public static P2PSwapLink ReadFromStream(DataInputStream zIn) throws IOException {
//        P2PSwapLink data = new P2PSwapLink();
//        data.readDataStream(zIn);
//        return data;
//    }
//}
