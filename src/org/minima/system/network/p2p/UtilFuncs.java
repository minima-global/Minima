package org.minima.system.network.p2p;

import java.net.InetSocketAddress;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

import org.minima.system.network.minima.NIOClientInfo;
import org.minima.utils.MinimaLogger;

public class UtilFuncs {

    private UtilFuncs(){}

    public static NIOClientInfo getClientFromInetAddress(InetSocketAddress address, List<NIOClientInfo> clients, boolean isIncoming) {
        NIOClientInfo returnClient = null;
        if (address != null) {
            for (NIOClientInfo client : clients) {
                if ((client != null)) {
                    if (new InetSocketAddress(client.getHost(), client.getPort()).equals(address) && client.isIncoming() == isIncoming) {
                        returnClient = client;
                        break;
                    }
                } else {
                    MinimaLogger.log("[-] client is null");
                }
            }
        } else {
            MinimaLogger.log("[-] address is null");
        }
        return returnClient;
    }

    public static NIOClientInfo getClientFromInetAddressEitherDirection(InetSocketAddress address, List<NIOClientInfo> clients) {
        NIOClientInfo returnClient = null;
        if (address != null) {
            for (NIOClientInfo client : clients) {
                if ((client != null)) {
                    if (new InetSocketAddress(client.getHost(), client.getPort()).equals(address)) {
                        returnClient = client;
                        break;
                    }
                } else {
                    MinimaLogger.log("[-] client is null");
                }
            }
        } else {
            MinimaLogger.log("[-] address is null");
        }
        return returnClient;
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
