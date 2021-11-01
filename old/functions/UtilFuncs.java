package org.minima.system.network.p2p.old.functions;

import org.minima.system.network.base.MinimaClient;
import org.minima.utils.MinimaLogger;

import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.concurrent.ThreadLocalRandom;

public class UtilFuncs {

    public static MinimaClient getClientForInetAddress(InetSocketAddress address, ArrayList<MinimaClient> clients, boolean isIncoming) {


        MinimaClient returnClient = null;
        if (address != null) {
            for (MinimaClient client : clients) {
                if (client != null)
                {
                    if (client.getMinimaAddress() != null)
                    {
                        if(client.getMinimaAddress().equals(address) && client.isIncoming() == isIncoming) {

                            returnClient = client;
                            break;
                        }
                    } else {
                        MinimaLogger.log("[-] Minima Address is null for client: " + client.getUID());
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


    public static MinimaClient getClientForInetAddressEitherDirection(InetSocketAddress address, ArrayList<MinimaClient> clients) {
        MinimaClient returnClient = null;
        if (address != null) {
            for (MinimaClient client : clients) {
                if (client != null)
                {
                    if (client.getMinimaAddress() != null)
                    {
                        if(client.getMinimaAddress().equals(address)) {

                            returnClient = client;
                            break;
                        }
                    } else {
                        MinimaLogger.log("[-] Minima Address is null for client: " + client.getUID());
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

    public static MinimaClient getMinimaClientForClient(InetSocketAddress address, ArrayList<MinimaClient> clients) {
        MinimaClient returnClient = null;
        if (address != null) {
            for (MinimaClient client : clients) {
                    if (client.getMinimaAddress() != null && client.isClient() && client.getMinimaAddress().equals(address))
                    {
                            returnClient = client;
                    } else {
                        MinimaLogger.log("[-] Minima Address is null for client: " + client.getUID());
                    }
            }
        } else {
            MinimaLogger.log("[-] address is null");
        }
        return returnClient;
    }

    public static InetSocketAddress SelectRandomAddress(ArrayList<InetSocketAddress> addresses){
        assert !addresses.isEmpty() : "Attempting to select from an empty list";
        InetSocketAddress returnAddress = null;
        if (!addresses.isEmpty()) {
            int idx = 0;
            if (addresses.size() > 1) {
                idx = ThreadLocalRandom.current().nextInt(addresses.size() - 1);
            }
            returnAddress = addresses.get(idx);
        }
        return  returnAddress;
    }
}
