package org.minima.system.network.p2p.functions;

import org.minima.GlobalParams;
import org.minima.system.network.base.MinimaClient;
import org.minima.system.network.p2p.P2PState;
import org.minima.system.network.p2p.messages.P2PMsgWalkLinks;
import org.minima.utils.messages.Message;

import java.net.InetSocketAddress;
import java.util.ArrayList;

public class DisconnectionFuncs {

    public static Message onInLinkDisconnected(P2PState state, MinimaClient client, ArrayList<MinimaClient> minimaClients) {
        Message returnMessage = null;
        if (state.isSetupComplete() && !state.isClient() && !state.getOutLinks().isEmpty() && state.getInLinks().size() < GlobalParams.P2P_NUM_LINKS) {
            // Replace Inlink
            P2PMsgWalkLinks walkLinks = new P2PMsgWalkLinks(false, false);
            InetSocketAddress nextHop = UtilFuncs.SelectRandomAddress(state.getOutLinks());
            if (nextHop == null) {
                nextHop = UtilFuncs.SelectRandomAddress(state.getInLinks());
            }
            MinimaClient minimaClient = UtilFuncs.getClientForInetAddress(nextHop, minimaClients, false);
            returnMessage = WalkLinksFuncs.genP2PWalkLinkMsg(state, minimaClient, walkLinks, "P2P_ON_DISCONNECTED");

        }
        if (client != null) {
            state.removeDisconnectingClient(client.getUID());
            state.removeLink(client);
        }
        return returnMessage;
    }

    public static Message onOutLinkDisconnected(P2PState state, MinimaClient client, ArrayList<MinimaClient> minimaClients) {

        Message returnMessage = null;
        if (state.isSetupComplete() && !state.isClient() && !state.getInLinks().isEmpty() && state.getOutLinks().size() < GlobalParams.P2P_NUM_LINKS) {
            // Replace Outlink
            P2PMsgWalkLinks walkLinks = new P2PMsgWalkLinks(true, false);
            InetSocketAddress nextHop = UtilFuncs.SelectRandomAddress(state.getInLinks());
            MinimaClient minimaClient = UtilFuncs.getClientForInetAddress(nextHop, minimaClients, true);
            returnMessage = WalkLinksFuncs.genP2PWalkLinkMsg(state, minimaClient, walkLinks, "P2P_ON_DISCONNECTED");
        }

        state.removeDisconnectingClient(client.getUID());
        state.removeLink(client);
        return returnMessage;
    }
}
