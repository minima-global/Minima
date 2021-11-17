package org.minima.system.network.p2p;

import java.net.InetSocketAddress;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ThreadLocalRandom;

import org.minima.system.network.minima.NIOClientInfo;

public class UtilFuncs {

    private UtilFuncs(){}

    public static NIOClientInfo searchLinksMapForAddress(InetSocketAddress address, Map<String, InetSocketAddress> links){
        for (String uid:links.keySet()){
            if (links.get(uid).equals(address)){
                return P2PFunctions.getNIOCLientInfo(uid);
            }
        }
        return null;
    }

    public static NIOClientInfo getClientFromInetAddress(InetSocketAddress address, P2PState state) {
        NIOClientInfo clientInfo;
        clientInfo = searchLinksMapForAddress(address, state.getInLinks());
        if (clientInfo == null){
            clientInfo = searchLinksMapForAddress(address, state.getOutLinks());
        }
        if (clientInfo == null){
            clientInfo = searchLinksMapForAddress(address, state.getNotAcceptingConnP2PLinks());
        }
        if (clientInfo == null){
            clientInfo = searchLinksMapForAddress(address, state.getNoneP2PLinks());
        }
        return clientInfo;
    }

    public static InetSocketAddress selectRandomAddress(List<InetSocketAddress> addresses) {
        InetSocketAddress returnAddress = null;
        if (!addresses.isEmpty()) {
            int idx = 0;
            if (addresses.size() > 1) {
                idx = ThreadLocalRandom.current().nextInt(addresses.size() - 1);
            }
            returnAddress = addresses.get(idx);
        }
        return returnAddress;
    }
}
