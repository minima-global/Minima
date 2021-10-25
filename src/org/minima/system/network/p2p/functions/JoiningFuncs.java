package org.minima.system.network.p2p.functions;

import lombok.extern.slf4j.Slf4j;
import org.minima.GlobalParams;
import org.minima.system.network.base.MinimaClient;
import org.minima.system.network.p2p.ConnectionDetails;
import org.minima.system.network.p2p.ConnectionReason;
import org.minima.system.network.p2p.P2PMessageProcessor;
import org.minima.system.network.p2p.P2PState;
import org.minima.system.network.p2p.messages.ExpiringMessage;
import org.minima.system.network.p2p.messages.P2PMsgWalkLinks;
import org.minima.utils.messages.Message;

import java.net.InetSocketAddress;
import java.util.ArrayList;

@Slf4j
public class JoiningFuncs {
    public static ArrayList<Message> joinRendezvousNode(P2PState state, ArrayList<MinimaClient> clients)
    {
        ArrayList<Message> msgs = new ArrayList<>();
        if (!state.getRecentJoiners().isEmpty()) {
            InetSocketAddress address = UtilFuncs.SelectRandomAddress(new ArrayList<>(state.getRecentJoiners()));
            msgs.add(new Message(P2PMessageProcessor.P2P_CONNECT).addObject("address", address).addString("reason", "RENDEZVOUS connection"));
            state.getConnectionDetailsMap().put(address, new ConnectionDetails(ConnectionReason.RENDEZVOUS));
        } else {
            log.error("No nodes to Rendezvous with - RandomNodeSet is empty");
        }
        return msgs;
    }

    public static ArrayList<Message> joinEntryNode(P2PState state, ArrayList<MinimaClient> clients)
    {
        ArrayList<Message> msgs = new ArrayList<>();
        if (!state.getRecentJoiners().isEmpty()) {
            InetSocketAddress address = UtilFuncs.SelectRandomAddress(new ArrayList<>(state.getRecentJoiners()));
            msgs.add(new Message(P2PMessageProcessor.P2P_CONNECT).addObject("address", address).addString("reason", "ENTRY_NODE connection"));
            state.getConnectionDetailsMap().put(address, new ConnectionDetails(ConnectionReason.ENTRY_NODE));
            state.setEntryNodeConnected(true);
        } else {
            log.error("No nodes to Rendezvous with - RandomNodeSet is empty");
        }

        return msgs;
    }

    public static ArrayList<Message> joinScaleOutLinks(P2PState state, ArrayList<MinimaClient> clients)
    {
        ArrayList<Message> msgs = new ArrayList<>();
        if(state.getOutLinks().size() < GlobalParams.P2P_NUM_LINKS) {
            int numActiveWalks = countActiveWalkLinks(state, true, true);
            boolean createWalk = state.getOutLinks().size() - numActiveWalks > 0;
            if (createWalk) {
                P2PMsgWalkLinks walkLinks = new P2PMsgWalkLinks(true, true);
                walkLinks.addHopToPath(state.getAddress());
                InetSocketAddress nextHop = UtilFuncs.SelectRandomAddress(state.getOutLinks());
                MinimaClient minimaClient = UtilFuncs.getClientForInetAddressEitherDirection(nextHop, clients);
                msgs.add(WalkLinksFuncs.genP2PWalkLinkMsg(state, minimaClient, walkLinks, "P2P_JOIN"));
            }
        }
        return msgs;
    }

    public static ArrayList<Message> requestInLinks(P2PState state, ArrayList<MinimaClient> clients)
    {
        ArrayList<Message> msgs = new ArrayList<>();
        if(state.getInLinks().size() < GlobalParams.P2P_NUM_LINKS) {
            int numActiveWalks = countActiveWalkLinks(state, false, false);
            boolean createWalk = state.getInLinks().size() - numActiveWalks > 0;
            if (createWalk) {
                // Replace Inlink
                P2PMsgWalkLinks walkLinks = new P2PMsgWalkLinks(false, false);
                walkLinks.addHopToPath(state.getAddress());
                InetSocketAddress nextHop = UtilFuncs.SelectRandomAddress(state.getOutLinks());
                MinimaClient minimaClient = UtilFuncs.getClientForInetAddressEitherDirection(nextHop, clients);
                msgs.add(WalkLinksFuncs.genP2PWalkLinkMsg(state, minimaClient, walkLinks, "P2P_JOIN"));
            }
        }
        return msgs;
    }
    public static int countActiveWalkLinks(P2PState state, boolean isInLinksWalk, boolean isJoiningWalk){
        int numActiveMessages = 0;
        for(ExpiringMessage expiringMessage: state.getExpiringMessageMap().values()){
            Message message = expiringMessage.getMsg();
            if (message.isMessageType(P2PMessageProcessor.P2P_WALK_LINKS)) {
                P2PMsgWalkLinks walkLinks = (P2PMsgWalkLinks) message.getObject("data");
                if (walkLinks.isWalkInLinks() == isInLinksWalk && walkLinks.isJoiningWalk() == isJoiningWalk) {
                    numActiveMessages += 1;
                }
            }
        }
        return numActiveMessages;
    }
}
