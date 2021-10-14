package org.minima.system.network.p2p.functions;

import lombok.extern.slf4j.Slf4j;
import org.minima.system.network.base.MinimaClient;
import org.minima.system.network.p2p.P2PState;
import org.minima.system.network.p2p.messages.P2PMsgIsClient;
import org.minima.utils.messages.Message;

import java.util.ArrayList;
import java.util.List;

import static java.util.stream.Collectors.toList;
import static org.minima.system.network.base.MinimaClient.NETMESSAGE_P2P_IS_CLIENT_CHANGE;
import static org.minima.system.network.p2p.P2PMessageProcessor.P2P_SEND_MESSAGE;
import static org.minima.system.network.p2p.functions.UtilFuncs.getClientForInetAddress;

@Slf4j
public class BroadcastFuncs {

    public static List<Message> broadcastIsClientChange(P2PState state, ArrayList<MinimaClient> clients) {

        return state.getOutLinks().stream()
                .map(outLink -> new Message(P2P_SEND_MESSAGE)
                .addObject("client", getClientForInetAddress(outLink, clients, false))
                .addObject("message", new Message(NETMESSAGE_P2P_IS_CLIENT_CHANGE).addObject("data", new P2PMsgIsClient(state.isClient(), state.getAddress()))))
                .collect(toList());
    }
}
