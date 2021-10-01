package org.minima.system.network.p2p.functions;

import lombok.extern.slf4j.Slf4j;
import org.minima.system.network.base.MinimaClient;
import org.minima.system.network.p2p.P2PMessageProcessor;
import org.minima.system.network.p2p.P2PState;
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

    public static Message onOutLinkWalkMsg(P2PState state, P2PMsgWalkLinks p2pWalkLinks, ArrayList<MinimaClient> allClients){
        Message retMsg;
        p2pWalkLinks.addHopToPath(state.getAddress());
        ArrayList<InetSocketAddress> filteredOutLinks = (ArrayList<InetSocketAddress>) state.getOutLinks().stream()
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
            P2PMsgSwapLink swapLink = new P2PMsgSwapLink();
            swapLink.setSecret(msgWalkLinks.getSecret());
            swapLink.setSwapTarget(msgWalkLinks.getPathTaken().get(0));
            swapLink.setConditionalSwapReq(true);

            retMsg = new Message(P2PMessageProcessor.P2P_SWAP_LINK)
                    .addObject("data", swapLink);

        }
        return retMsg;
    }

    public static Message onInLinkWalkMsg(P2PState state, P2PMsgWalkLinks p2pWalkLinks, ArrayList<MinimaClient> allClients){
        Message retMsg = null;
        p2pWalkLinks.addHopToPath(state.getAddress());
        ArrayList<InetSocketAddress> filteredInLinks = (ArrayList<InetSocketAddress>) state.getInLinks().stream()
                .filter(x -> p2pWalkLinks.getPathTaken().stream().noneMatch(x::equals))
                .collect(Collectors.toList());
        InetSocketAddress nextHop = UtilFuncs.SelectRandomAddress(filteredInLinks);

        if (nextHop != null) {
            retMsg = createNextHopMsg(nextHop, p2pWalkLinks, allClients);
        } else {
            p2pWalkLinks.setReturning(true);
            retMsg = onWalkLinkResponseMsg(state, p2pWalkLinks, allClients);
        }

        return retMsg;
    }

    public static Message createNextHopMsg(InetSocketAddress nextHop, P2PMsgWalkLinks p2pWalkLinks, ArrayList<MinimaClient> allClients){
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

    public static Message onWalkLinkResponseMsg(P2PState state, P2PMsgWalkLinks p2pWalkLinks, ArrayList<MinimaClient> allClients){
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
            if (!state.getRandomNodeSet().contains(connectTargetAddress)) {
                state.addRandomNodeSet(connectTargetAddress);
            }
            if (state.getOutLinks().size() < state.getNumLinks()) {
                if (msg.isJoiningWalk()) {
                    state.addRequestSwapOnConnect(connectTargetAddress);
                }
                returnMessage = new Message(P2PMessageProcessor.P2P_CONNECT)
                        .addObject("address", connectTargetAddress)
                        .addString("reason", "For completed connection walk");
            } else {
                log.debug("[!] P2P WalkLinks Result: Not Connecting already have max numLinks");
            }
        } else {
            log.debug("[!] P2P WalkLinks Result: Not Connecting as returned own address");
        }

        return returnMessage;
    }
}
