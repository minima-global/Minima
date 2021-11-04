package org.minima.system.network.p2p;

import org.minima.system.network.minima.NIOClientInfo;
import org.minima.system.network.p2p.messages.P2PDoSwap;
import org.minima.system.network.p2p.messages.P2PWalkLinks;
import org.minima.utils.MinimaLogger;
import org.minima.utils.messages.Message;

import java.net.InetSocketAddress;
import java.util.*;
import java.util.stream.Collectors;

/*
 * CLASS TO HOLD NO STATE. FUNCTIONS NOT STATIC FOR EASE OF TESTING
 * CAUSE JAVA....
 */
public class WalkLinksFuncs {

    private WalkLinksFuncs() {
    }

    /**
     * Removes all emements in List B from the values in hashmap A
     * returning a list of the remaining values or an empty list
     * @param a Links Map of uid to Address
     * @param b List of address to remove from a
     * @return Address remaining in a or an empty list
     */
    public static List<InetSocketAddress> removeIPsInBFromA(Map<String, InetSocketAddress> a, List<InetSocketAddress> b){
        return a.values().stream()
                .filter(x -> b.stream().noneMatch(x::equals))
                .collect(Collectors.toList());
    }

    /**
     * Process and OutLinks Walk from a WalkLinks Msg
     * 1) Adds the previous address to the path
     * 2) Gets the next hop from a list of outLinks that are not already in the path
     * 3) If there is a next hop and the path isn't 10 hops
     *     - Creates the next hop message
     *    Else:
     *     - generate a do swap message for one of this node's inbound connections to
     *       connect to the node that sent this OutLinks message
     * @param state the state of the p2p system
     * @param p2pWalkLinks the walkLinks message that is being processed
     * @param clientMsgIsFrom the client the message came from
     * @param tgtNumLinks the target number of links this node is aiming to maintain
     * @param allClients all the clients this node is connected too
     * @return null or a message to be sent the client with the specified uid
     */
    public static Message onOutLinkWalkMsg(P2PState state, P2PWalkLinks p2pWalkLinks, NIOClientInfo clientMsgIsFrom, int tgtNumLinks, List<NIOClientInfo> allClients) {
        Message retMsg;
        // Add the previous address to the list
        p2pWalkLinks.addHopToPath(new InetSocketAddress(clientMsgIsFrom.getHost(), clientMsgIsFrom.getPort()));

        List<InetSocketAddress> filteredOutLinks = removeIPsInBFromA(state.getOutLinks(), p2pWalkLinks.getPathTaken());

        InetSocketAddress nextHop = UtilFuncs.selectRandomAddress(filteredOutLinks);

        if (p2pWalkLinks.getPathTaken().size() < 9 && nextHop != null) {
            retMsg = createNextHopMsg(nextHop, p2pWalkLinks, allClients);
        } else {
            retMsg = generateDoSwapMessageFromWalk(state, p2pWalkLinks, tgtNumLinks);
        }

        return retMsg;
    }

    /**
     * Creates a message with the data needed to send the walkLinks message onto the next hop
     * @param destinationAddress destination ip for the walkLinks msg
     * @param p2pWalkLinks the walkLinks message to send on
     * @param allClients list of all connected clients
     * @return null if there is no client for the destination Ip
     */
    public static Message createNextHopMsg(InetSocketAddress destinationAddress, P2PWalkLinks p2pWalkLinks, List<NIOClientInfo> allClients) {
        Message retMsg = null;
        NIOClientInfo client = UtilFuncs.getClientFromInetAddressEitherDirection(destinationAddress, allClients);
        if (client != null) {
            retMsg = new Message()
                    .addString("uid", client.getUID())
                    .addObject("json", p2pWalkLinks.toJson());
        } else {
            MinimaLogger.log("[-] P2P_WALK_LINKS NIOClientInfo for " + destinationAddress + " does not exist");
        }
        return retMsg;
    }

    /**
     * Generates a doswap message if this node has more
     * inlinks than the tgtNumLinks
     * @param state The P2P systems state
     * @param msgWalkLinks The walkLinks message that has trigger generation of a doswap
     * @param tgtNumLinks
     * @return
     */
    public static Message generateDoSwapMessageFromWalk(P2PState state, P2PWalkLinks msgWalkLinks, int tgtNumLinks) {
        Message msg = null;
        Random random = new Random();
        if (state.getInLinks().size() > tgtNumLinks) {
            msg = new Message();
            List<String> uids = new ArrayList<>(state.getInLinks().keySet());
            String swappingPeerUID = uids.get(random.nextInt(uids.size() -1));
            P2PDoSwap swapLink = new P2PDoSwap(
                    msgWalkLinks.getSecret(),
                    msgWalkLinks.getPathTaken().get(0),
                    swappingPeerUID
            );
            msg.addString("uid", swappingPeerUID);
            msg.addObject("json", swapLink.toJson());
        }
        return msg;
    }

    /**
     * Process and InLinks Walk from a WalkLinks Msg
     * 1) Adds the previous address to the path
     * 2) Gets the next hop from a list of inLinks that are not already in the path
     * 3) If there is a next hop and the path isn't 10 hops
     *     - Creates the next hop message
     *    Else:
     *     - adds the number of available client slots to the message and sends it back
     * @param state the state of the p2p system
     * @param p2pWalkLinks the walkLinks message that is being processed
     * @param clientMsgIsFrom the client the message came from
     * @param tgtNumNoneP2PConn the target number of none p2p connections this node is aiming to maintain
     * @param allClients all the clients this node is connected too
     * @return null or a message to be sent the client with the specified uid
     */
    public static Message onInLinkWalkMsg(P2PState state, P2PWalkLinks p2pWalkLinks, NIOClientInfo clientMsgIsFrom, int tgtNumNoneP2PConn, List<NIOClientInfo> allClients) {
        Message retMsg;
        // Add the previous address to the list
        p2pWalkLinks.addHopToPath(new InetSocketAddress(clientMsgIsFrom.getHost(), clientMsgIsFrom.getPort()));
        List<InetSocketAddress> filteredInLinks = removeIPsInBFromA(state.getInLinks(), p2pWalkLinks.getPathTaken());
        InetSocketAddress nextHop = UtilFuncs.selectRandomAddress(filteredInLinks);

        if (p2pWalkLinks.getPathTaken().size() < 9 && nextHop != null) {
            retMsg = createNextHopMsg(nextHop, p2pWalkLinks, allClients);
        } else {
            p2pWalkLinks.addHopToPath(state.getMyMinimaAddress());
            p2pWalkLinks.setReturning(true);
            if (p2pWalkLinks.isClientWalk()) {
                p2pWalkLinks.setAvailableClientSlots(tgtNumNoneP2PConn - state.getNoneP2PLinks().size());
            }
            retMsg = onWalkLinkResponseMsg(state, p2pWalkLinks, allClients);
        }

        return retMsg;
    }


    /**
     * Generates the message required to send the walkLink message back down the path
     * @param state the p2p system state
     * @param p2pWalkLinks the walkLinks Message that is being sent back
     * @param allClients all clients this node is connected too
     * @return null if there is no client for the previous or message to be sent back
     */
    public static Message onWalkLinkResponseMsg(P2PState state, P2PWalkLinks p2pWalkLinks, List<NIOClientInfo> allClients) {
        Message retMsg = null;
        InetSocketAddress nextHop = p2pWalkLinks.getPreviousNode(state.getMyMinimaAddress());
        NIOClientInfo client = UtilFuncs.getClientFromInetAddressEitherDirection(nextHop, allClients);
        if (client != null) {

            retMsg = new Message()
                    .addString("uid", client.getUID())
                    .addObject("json", p2pWalkLinks.toJson());
        }
        return retMsg;
    }

    /**
     * Process a InLink walk when its returns to its origin
     * 1) adds the all new peers from path to known peers list
     * 2) If this node still needs outLinks returns the address
     *        of the final node in the path to be connected too
     *    Else returns null
     * @param state the p2p systems state
     * @param walkLinks the returned walkLinks message
     * @param tgtNumOutLinks the target number of outLink connections the node wants to maintain
     * @return the address of a node to connect too or null if were not to connect to a node
     */
    public static InetSocketAddress onReturnedWalkMsg(P2PState state, P2PWalkLinks walkLinks, int tgtNumOutLinks) {
        InetSocketAddress connectTargetAddress = null;
        if (state.getOutLinks().size() < tgtNumOutLinks) {
            if (!walkLinks.getPathTaken().get(walkLinks.getPathTaken().size() - 1).equals(state.getMyMinimaAddress())) {
                state.getKnownPeers().addAll(walkLinks.getPathTaken());
                connectTargetAddress = walkLinks.getPathTaken().get(walkLinks.getPathTaken().size() - 1);
            } else {
                MinimaLogger.log("[!] P2P_WALK_LINKS_RESPONSE: Not Connecting as returned own address");
            }
        } else {
            MinimaLogger.log("[!] P2P_WALK_LINKS_RESPONSE: Not Connecting already have max numLinks");
        }

        return connectTargetAddress;
    }

//    public static List<Message> onReturnedClientWalkMsg(P2PState state, P2PWalkLinks msg) {
//        List<Message> returnMessage = new List<>();
//        InetSocketAddress connectTargetAddress = msg.getPathTaken().get(msg.getPathTaken().size() - 1);
//        if (!connectTargetAddress.equals(state.getMyMinimaAddress())) {
//            state.getKnownPeers().add(connectTargetAddress);
//            MinimaLogger.log("[!] P2P_WALK_LINKS_RESPONSE: CLIENT creating do swap messages");
//            returnMessage.addAll(GreetingFuncs.genClientLoadBalanceRequests(state, connectTargetAddress, msg.getAvailableClientSlots()));
//
//        } else {
//            MinimaLogger.log("[!] P2P_WALK_LINKS_RESPONSE: Not Connecting as returned own address");
//        }
//
//        return returnMessage;
//    }


//    public static Message onSwapClientsReq(P2PState state, P2PMsgSwapLink swapLink, List<MinimaClient> allClients) {
//        InetSocketAddress addressToDoSwap = UtilFuncs.SelectRandomAddress(state.getClientLinks());
//
//        List<MinimaClient> clients = allClients.stream()
//                .filter(MinimaClient::isClient)
//                .collect(Collectors.toCollection(List::new));
//
//        MinimaLogger.log("[!] P2P_SWAP_LINK Num Clients post filter: " + clients.size() + " num disconnecting clients: " + state.getDisconnectingClients().size() + " num clients: " + allClients.size());
//        MinimaClient minimaClient = UtilFuncs.getClientForInetAddress(addressToDoSwap, clients, true);
//        return SwapFuncs.generateDoSwapMessage(state, swapLink, minimaClient);
//    }
//
//    public static Message onSwapReq(P2PState state, P2PMsgSwapLink swapLink, List<MinimaClient> allClients) {
//        MinimaClient minimaClient = null;
//
//
//        // SwapTarget
//        List<InetSocketAddress> filteredInLinks = (List<InetSocketAddress>) state.getInLinks().stream()
//                .filter(x -> !x.equals(swapLink.getSwapTarget()))
//                .distinct()
//                .collect(Collectors.toList());
//
//        // This isn't correct, need to have
//        if (!filteredInLinks.isEmpty()) {
//            InetSocketAddress addressToDoSwap = UtilFuncs.SelectRandomAddress(filteredInLinks);
//
//            List<MinimaClient> clients = allClients.stream()
//                    .filter(x -> !state.getDisconnectingClients()
//                            .contains(x.getUID())).collect(Collectors.toCollection(List::new));
//            MinimaLogger.log("[!] P2P_SWAP_LINK  from: " + state.getAddress() + " Num Clients post filter: " + clients.size() + " num disconnecting clients: " + state.getDisconnectingClients().size() + " num clients: " + allClients.size());
//            minimaClient = UtilFuncs.getClientForInetAddress(addressToDoSwap, clients, true);
//        }
//        return SwapFuncs.generateDoSwapMessage(state, swapLink, minimaClient);
//    }
}
