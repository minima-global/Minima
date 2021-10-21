package org.minima.system.network.p2p.functions;

import org.minima.system.network.base.MinimaClient;
import org.minima.system.network.p2p.P2PState;
import org.minima.system.network.p2p.Traceable;
import org.minima.system.network.p2p.messages.P2PMsgWalkLinks;
import org.minima.utils.messages.Message;

import java.net.InetSocketAddress;
import java.util.ArrayList;

public class DisconnectionFuncs {

    public static Message onInLinkDisconnected(P2PState state, MinimaClient client, ArrayList<MinimaClient> minimaClients, Traceable traceable) {
        Message returnMessage = null;
        if (state.isSetupComplete() && !state.isClient() && !state.getOutLinksCopy().isEmpty() && state.getInLinksCopy().size() < state.getNumLinks()) {
            // Replace Inlink
            P2PMsgWalkLinks walkLinks = new P2PMsgWalkLinks(false, false, traceable);
            InetSocketAddress nextHop = UtilFuncs.SelectRandomAddress(state.getOutLinksCopy());
            if (nextHop == null) {
                nextHop = UtilFuncs.SelectRandomAddress(state.getInLinksCopy());
            }
            MinimaClient minimaClient = UtilFuncs.getClientForInetAddress(nextHop, minimaClients, false);
            returnMessage = WalkLinksFuncs.genP2PWalkLinkMsg(state, minimaClient, walkLinks, "P2P_ON_DISCONNECTED", traceable);

        }
        if (client != null) {
            state.removeDisconnectingClient(client.getUID(), traceable);
            state.removeLink(client, traceable);
        }
        return returnMessage;
    }

    public static Message onOutLinkDisconnected(P2PState state, MinimaClient client, ArrayList<MinimaClient> minimaClients, Traceable traceable) {

        Message returnMessage = null;
        if (state.isSetupComplete() && !state.isClient() && !state.getInLinksCopy().isEmpty() && state.getOutLinksCopy().size() < state.getNumLinks()) {
            // Replace Outlink
            P2PMsgWalkLinks walkLinks = new P2PMsgWalkLinks(true, false, traceable);
            InetSocketAddress nextHop = UtilFuncs.SelectRandomAddress(state.getInLinksCopy());
            MinimaClient minimaClient = UtilFuncs.getClientForInetAddress(nextHop, minimaClients, true);
            returnMessage = WalkLinksFuncs.genP2PWalkLinkMsg(state, minimaClient, walkLinks, "P2P_ON_DISCONNECTED", traceable);
        }

        state.removeDisconnectingClient(client.getUID(), traceable);
        state.removeLink(client, traceable);
        return returnMessage;
    }
}
