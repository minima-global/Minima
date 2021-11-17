package org.minima.system.network.p2p.testingutils;

import org.minima.system.network.minima.NIOClientInfo;

import java.util.ArrayList;
import java.util.List;

public class QuickClients {

    public static List<NIOClientInfo> generateClientInfoList(String prefix, int numElements, int port, String uidPrefix, boolean isIncoming) {
        List<NIOClientInfo> outList = new ArrayList<>();
        for (int i = 0; i < numElements; i++) {
            outList.add(new NIOClientInfo(uidPrefix.concat(Integer.toString(i + 1)), prefix.concat(String.valueOf(i + 1)), port, isIncoming));
        }
        return outList;
    }

}
