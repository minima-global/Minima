package org.minima.system.network.p2p.functions;

import lombok.extern.slf4j.Slf4j;
import org.minima.system.network.base.MinimaClient;
import org.minima.system.network.p2p.*;
import org.minima.system.network.p2p.messages.ExpiringMessage;
import org.minima.system.network.p2p.messages.P2PMsgSwapLink;
import org.minima.system.network.p2p.messages.P2PMsgWalkLinks;
import org.minima.utils.messages.Message;

import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.stream.Collectors;

/*
 * CLASS TO HOLD NO STATE. FUNCTIONS NOT STATIC FOR EASE OF TESTING
 * CAUSE JAVA....
 */
@Slf4j
public class WalkLinksFuncs {

    public static Message onOutLinkWalkMsg(P2PState state, P2PMsgWalkLinks p2pWalkLinks, ArrayList<MinimaClient> allClients) {
        Message retMsg;
        p2pWalkLinks.addHopToPath(state.getAddress());
        ArrayList<InetSocketAddress> filteredOutLinks = (ArrayList<InetSocketAddress>) state.getOutLinksCopy().stream()
                .filter(x -> p2pWalkLinks.getPathTaken().stream().noneMatch(x::equals))
                .collect(Collectors.toList());
        InetSocketAddress nextHop = UtilFuncs.SelectRandomAddress(filteredOutLinks);

        if (nextHop != null) {
            retMsg = createNextHopMsg(nextHop, p2pWalkLinks, allClients);
        } else {
            retMsg = createSwapLinkMsg(p2pWalkLinks);
        }

        return retMsg;
    }
    public static Message createSwapLinkMsg(P2PMsgWalkLinks msgWalkLinks) {
        Message retMsg = null;
        if (!msgWalkLinks.isWalkInLinks()) {
            P2PMsgSwapLink swapLink = new P2PMsgSwapLink(msgWalkLinks.getSecret(),
                    msgWalkLinks.getPathTaken().get(0),
                    false,
                    true,
                    msgWalkLinks);

            retMsg = new Message(P2PMessageProcessor.P2P_SWAP_LINK)
                    .addObject("data", swapLink);

        }
        return retMsg;
    }

    public static Message onInLinkWalkMsg(P2PState state, P2PMsgWalkLinks p2pWalkLinks, ArrayList<MinimaClient> allClients) {
        Message retMsg;
        p2pWalkLinks.addHopToPath(state.getAddress());
        ArrayList<InetSocketAddress> filteredInLinks = (ArrayList<InetSocketAddress>) state.getInLinksCopy().stream()
                .filter(x -> p2pWalkLinks.getPathTaken().stream().noneMatch(x::equals))
                .collect(Collectors.toList());
        InetSocketAddress nextHop = UtilFuncs.SelectRandomAddress(filteredInLinks);

        if (nextHop != null) {
            retMsg = createNextHopMsg(nextHop, p2pWalkLinks, allClients);
        } else {
            // todo: need to check if we are a client or not
            p2pWalkLinks.setReturning(true);
            if (p2pWalkLinks.isClientWalk()){
                p2pWalkLinks.setAvailableClientSlots(state.getNumLinks() - state.getClientLinksCopy().size());
            }
            state.addExpectedAuthKeyAndValue(p2pWalkLinks.getSecret().toString(), System.currentTimeMillis() + P2PState.AUTH_KEY_EXPIRY, p2pWalkLinks);
            retMsg = onWalkLinkResponseMsg(state, p2pWalkLinks, allClients);
        }

        return retMsg;
    }

    public static Message createNextHopMsg(InetSocketAddress nextHop, P2PMsgWalkLinks p2pWalkLinks, ArrayList<MinimaClient> allClients) {
        Message retMsg = null;
        MinimaClient minimaClient = UtilFuncs.getClientForInetAddressEitherDirection(nextHop, allClients);
        if (minimaClient != null) {
            log.debug("[+] P2P_WALK_LINKS Sending message to: " + nextHop + " isReturning: " + p2pWalkLinks.isReturning());

            Message message = new Message(MinimaClient.NETCLIENT_P2P_WALK_LINKS)
                    .addObject("data", p2pWalkLinks);

            retMsg = new Message(P2PMessageProcessor.P2P_SEND_MESSAGE)
                    .addObject("client", minimaClient)
                    .addObject("message", message);
        } else {
            log.warn("[-] P2P_WALK_LINKS MinimaClient for " + nextHop + " does not exist");
        }
        return retMsg;
    }

    public static Message onWalkLinkResponseMsg(P2PState state, P2PMsgWalkLinks p2pWalkLinks, ArrayList<MinimaClient> allClients) {
        Message retMsg = null;
        InetSocketAddress nextHop = p2pWalkLinks.getPreviousNode(state.getAddress());
        MinimaClient minimaClient = UtilFuncs.getClientForInetAddressEitherDirection(nextHop, allClients);
        if (minimaClient != null) {

            Message message = new Message(MinimaClient.NETCLIENT_P2P_WALK_LINKS_RESPONSE)
                    .addObject("data", p2pWalkLinks);

            retMsg = new Message(P2PMessageProcessor.P2P_SEND_MESSAGE)
                    .addObject("client", minimaClient)
                    .addObject("message", message);
        }
        return retMsg;
    }

    public static Message onReturnedWalkMsg(P2PState state, P2PMsgWalkLinks msg) {
        Message returnMessage = null;
        InetSocketAddress connectTargetAddress = msg.getPathTaken().get(msg.getPathTaken().size() - 1);
        if (!connectTargetAddress.equals(state.getAddress())) {
            if (!state.getRecentJoinersCopy().contains(connectTargetAddress)) {
                state.addRandomNodeSet(connectTargetAddress, msg);
            }
            if (state.getOutLinksCopy().size() < state.getNumLinks()) {
                ConnectionReason reason = ConnectionReason.REPLACING_OUT_LINK;
                if (msg.isJoiningWalk()) {
                    reason = ConnectionReason.ADDING_OUT_LINK;
                    // On adding an outlink we also expect a do swap back to this node
                    state.addExpectedAuthKeyAndValue(msg.getSecret().toString(), System.currentTimeMillis() + P2PState.AUTH_KEY_EXPIRY, msg);
                }
                returnMessage = new Message(P2PMessageProcessor.P2P_CONNECT)
                        .addObject("address", connectTargetAddress)
                        .addString("reason", reason + " triggered by a completed connection walk");
                state.getConnectionDetailsMap().put(connectTargetAddress, new ConnectionDetails(reason, msg.getSecret()));
            } else {
                log.debug("[!] P2P_WALK_LINKS_RESPONSE: Not Connecting already have max numLinks");
            }
        } else {
            log.debug("[!] P2P_WALK_LINKS_RESPONSE: Not Connecting as returned own address");
        }

        return returnMessage;
    }

    public static ArrayList<Message> onReturnedClientWalkMsg(P2PState state, P2PMsgWalkLinks msg) {
        ArrayList<Message> returnMessage = new ArrayList<>();
        InetSocketAddress connectTargetAddress = msg.getPathTaken().get(msg.getPathTaken().size() - 1);
        if (!connectTargetAddress.equals(state.getAddress())) {
            if (!state.getRecentJoinersCopy().contains(connectTargetAddress)) {
                state.addRandomNodeSet(connectTargetAddress, msg);
            }
            log.debug("[!] P2P_WALK_LINKS_RESPONSE: CLIENT creating do swap messages");
            returnMessage.addAll(GreetingFuncs.genClientLoadBalanceRequests(state, connectTargetAddress, msg.getAvailableClientSlots(), msg));

        } else {
            log.debug("[!] P2P_WALK_LINKS_RESPONSE: Not Connecting as returned own address");
        }

        return returnMessage;
    }


    public static Message genP2PWalkLinkMsg(P2PState state, MinimaClient minimaClient, P2PMsgWalkLinks walkLinks, String logType, Traceable traceable) {
        Message retMsg = null;
        if (minimaClient != null) {
            if (walkLinks.isJoiningWalk()) {
                log.debug("[+] " + logType + " P2P_WALK Starting inLink walk add outLink neighbour");
            } else {
                if (walkLinks.isWalkInLinks()) {
                    log.debug("[+] " + logType + " P2P_WALK Starting inLink walk to replace lost outLink neighbour");
                } else {
                    log.debug("[+] " + logType + " P2P_WALK Starting outLink walk to replace lost inLink neighbour");
                }
            }
            retMsg = new Message(P2PMessageProcessor.P2P_SEND_MESSAGE, traceable);
            retMsg.addObject("client", minimaClient)
                    .addObject("message", new Message(MinimaClient.NETCLIENT_P2P_WALK_LINKS, retMsg).addObject("data", walkLinks));
            ExpiringMessage expiringMessage = new ExpiringMessage(new Message(P2PMessageProcessor.P2P_WALK_LINKS, retMsg).addObject("data", walkLinks));
            expiringMessage.setTimestamp(System.currentTimeMillis() + 5_000L);
            state.addExpiringMessage(walkLinks.getSecret(), expiringMessage, traceable);
            state.addExpectedAuthKeyAndValue(walkLinks.getSecret().toString(), System.currentTimeMillis() + P2PState.AUTH_KEY_EXPIRY, traceable);

        }
        return retMsg;
    }
}
