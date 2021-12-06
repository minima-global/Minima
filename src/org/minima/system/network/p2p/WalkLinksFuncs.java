package org.minima.system.network.p2p;

import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.stream.Collectors;

import org.minima.objects.base.MiniData;
import org.minima.system.network.minima.NIOClientInfo;
import org.minima.system.network.p2p.messages.P2PDoSwap;
import org.minima.system.network.p2p.messages.P2PWalkLinks;
import org.minima.utils.MinimaLogger;
import org.minima.utils.messages.Message;

/*
 * CLASS TO HOLD NO STATE. FUNCTIONS NOT STATIC FOR EASE OF TESTING
 * CAUSE JAVA....
 */
public class WalkLinksFuncs {

    private static final Random random = new Random();

    private WalkLinksFuncs() {
    }

    /**
     * Removes all emements in List B from the values in hashmap A
     * returning a list of the remaining values or an empty list
     *
     * @param a Links Map of uid to Address
     * @param b List of address to remove from a
     * @return Address remaining in a or an empty list
     */
    public static List<InetSocketAddress> removeIPsInBFromA(Map<String, InetSocketAddress> a, List<InetSocketAddress> b) {
        return a.values().stream()
                .filter(x -> b.stream().noneMatch(x::equals))
                .collect(Collectors.toList());
    }

    /**
     * Process and OutLinks Walk from a WalkLinks Msg
     * 1) Adds the previous address to the path
     * 2) Gets the next hop from a list of outLinks that are not already in the path
     * 3) If there is a next hop and the path isn't 10 hops
     * - Creates the next hop message
     * Else:
     * - generate a do swap message for one of this node's inbound connections to
     * connect to the node that sent this OutLinks message
     *
     * @param state           the state of the p2p system
     * @param p2pWalkLinks    the walkLinks message that is being processed
     * @param clientMsgIsFrom the client the message came from
     * @param tgtNumLinks     the target number of links this node is aiming to maintain
     * @param allClients      all the clients this node is connected too
     * @return null or a message to be sent the client with the specified uid
     */
    public static Message onOutLinkWalkMsg(P2PState state, P2PWalkLinks p2pWalkLinks, NIOClientInfo clientMsgIsFrom, int tgtNumLinks, List<NIOClientInfo> allClients) {
        Message retMsg;
        // Add the previous address to the list
        p2pWalkLinks.addHopToPath(getAddressFromUID(state, clientMsgIsFrom.getUID()));

        List<InetSocketAddress> filteredOutLinks = removeIPsInBFromA(state.getOutLinks(), p2pWalkLinks.getPathTaken());

        InetSocketAddress nextHop = UtilFuncs.selectRandomAddress(filteredOutLinks);

        if (p2pWalkLinks.getPathTaken().size() < 9 && nextHop != null) {
            retMsg = createNextHopMsg(nextHop, p2pWalkLinks, state);
        } else {
            retMsg = generateDoSwapMessageFromWalk(state, p2pWalkLinks, tgtNumLinks);
        }

        return retMsg;
    }

    /**
     * Creates a message with the data needed to send the walkLinks message onto the next hop
     *
     * @param destinationAddress destination ip for the walkLinks msg
     * @param p2pWalkLinks       the walkLinks message to send on
     * @return null if there is no client for the destination Ip
     */
    public static Message createNextHopMsg(InetSocketAddress destinationAddress, P2PWalkLinks p2pWalkLinks, P2PState state) {
        Message retMsg = null;
        NIOClientInfo client = UtilFuncs.getClientFromInetAddress(destinationAddress, state);
        if (client != null) {
            retMsg = new Message(P2PManager.P2P_SEND_MSG)
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
     *
     * @param state        The P2P systems state
     * @param msgWalkLinks The walkLinks message that has trigger generation of a doswap
     * @param tgtNumLinks
     * @return
     */
    public static Message generateDoSwapMessageFromWalk(P2PState state, P2PWalkLinks msgWalkLinks, int tgtNumLinks) {
        Message msg = null;

        if (state.getInLinks().size() > tgtNumLinks) {
            msg = new Message(P2PManager.P2P_SEND_MSG);
            List<String> uids = new ArrayList<>(state.getInLinks().keySet());
            String swappingPeerUID = uids.get(random.nextInt(uids.size() - 1));
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

    public static InetSocketAddress getAddressFromUID(P2PState state, String uid){
        InetSocketAddress address;
        address = state.getInLinks().get(uid);
        if (address == null){
            address = state.getOutLinks().get(uid);
        }
        if (address == null){
            address = state.getNotAcceptingConnP2PLinks().get(uid);
        }
        if (address == null){
            address = state.getNoneP2PLinks().get(uid);
        }
        if (address == null){
            MinimaLogger.log("No client found for: " + uid);
        }
        return address;
    }


    /**
     * Process and InLinks Walk from a WalkLinks Msg
     * 1) Adds the previous address to the path
     * 2) Gets the next hop from a list of inLinks that are not already in the path
     * 3) If there is a next hop and the path isn't 10 hops
     * - Creates the next hop message
     * Else:
     * - adds the number of available client slots to the message and sends it back
     *
     * @param state             the state of the p2p system
     * @param p2pWalkLinks      the walkLinks message that is being processed
     * @param clientMsgIsFrom   the client the message came from
     * @param allClients        all the clients this node is connected too
     * @return null or a message to be sent the client with the specified uid
     */
    public static Message onInLinkWalkMsg(P2PState state, P2PWalkLinks p2pWalkLinks, NIOClientInfo clientMsgIsFrom, List<NIOClientInfo> allClients) {
        Message retMsg;
        // Add the previous address to the list

        p2pWalkLinks.addHopToPath(getAddressFromUID(state, clientMsgIsFrom.getUID()));
        List<InetSocketAddress> filteredInLinks = removeIPsInBFromA(state.getInLinks(), p2pWalkLinks.getPathTaken());
        InetSocketAddress nextHop = UtilFuncs.selectRandomAddress(filteredInLinks);

        if (p2pWalkLinks.getPathTaken().size() < 9 && nextHop != null) {
            retMsg = createNextHopMsg(nextHop, p2pWalkLinks, state);
        } else {
            p2pWalkLinks.addHopToPath(state.getMyMinimaAddress());
            p2pWalkLinks.setReturning(true);
            if (p2pWalkLinks.isClientWalk()) {
                p2pWalkLinks.setAvailableNoneP2PConnectionSlots(state.getMaxNumNoneP2PConnections() - state.getNotAcceptingConnP2PLinks().size());
            }
            retMsg = onWalkLinkResponseMsg(state, p2pWalkLinks);
        }

        return retMsg;
    }


    /**
     * Generates the message required to send the walkLink message back down the path
     *
     * @param state        the p2p system state
     * @param p2pWalkLinks the walkLinks Message that is being sent back
     * @return null if there is no client for the previous or message to be sent back
     */
    public static Message onWalkLinkResponseMsg(P2PState state, P2PWalkLinks p2pWalkLinks) {
        Message retMsg = null;
        InetSocketAddress nextHop = p2pWalkLinks.getPreviousNode(state.getMyMinimaAddress());
        NIOClientInfo client = UtilFuncs.getClientFromInetAddress(nextHop, state);
        if (client != null) {

            retMsg = new Message(P2PManager.P2P_SEND_MSG)
                    .addString("uid", client.getUID())
                    .addObject("json", p2pWalkLinks.toJson());
        }
        return retMsg;
    }

    /**
     * Process a InLink walk when its returns to its origin
     * 1) adds the all new peers from path to known peers list
     * 2) If this node still needs outLinks returns the address
     * of the final node in the path to be connected too
     * Else returns null
     *
     * @param state          the p2p systems state
     * @param walkLinks      the returned walkLinks message
     * @param tgtNumOutLinks the target number of outLink connections the node wants to maintain
     * @return the address of a node to connect too or null if were not to connect to a node
     */
    public static List<Message> onReturnedWalkMsg(P2PState state, P2PWalkLinks walkLinks, int tgtNumOutLinks) {
        List<Message> retMsg = new ArrayList<>();
        if (state.getOutLinks().size() < tgtNumOutLinks) {
            if (!walkLinks.getPathTaken().get(walkLinks.getPathTaken().size() - 1).equals(state.getMyMinimaAddress())) {
                state.getKnownPeers().addAll(walkLinks.getPathTaken());
                state.getKnownPeers().remove(state.getMyMinimaAddress());
                InetSocketAddress connectTargetAddress = walkLinks.getPathTaken().get(walkLinks.getPathTaken().size() - 1);
                retMsg.add(new Message(P2PManager.P2P_SEND_CONNECT)
                        .addObject("address", connectTargetAddress));
            } else {
                MinimaLogger.log("[!] P2P_WALK_LINKS_RESPONSE: Not Connecting as returned own address");
            }
        } else {
            MinimaLogger.log("[!] P2P_WALK_LINKS_RESPONSE: Not Connecting already have max numLinks");
        }

        return retMsg;
    }

    public static List<Message> onReturnedLoadBalanceWalkMsg(P2PState state, P2PWalkLinks msg) {
        List<Message> returnMessage = new ArrayList<>();
        InetSocketAddress connectTargetAddress = msg.getPathTaken().get(msg.getPathTaken().size() - 1);
        if (!connectTargetAddress.equals(state.getMyMinimaAddress())) {
            state.getKnownPeers().add(connectTargetAddress);
            returnMessage.addAll(genLoadBalanceDoSwaps(state, connectTargetAddress, msg.getAvailableNoneP2PConnectionSlots()));

        } else {
            MinimaLogger.log("[!] P2P_WALK_LINKS_RESPONSE: Not Connecting as returned own address");
        }

        return returnMessage;
    }


    public static List<Message> genLoadBalanceDoSwaps(P2PState state, InetSocketAddress address, int maxClientsCanReceive) {

        List<Message> retMessages = new ArrayList<>();
        if (state.getNotAcceptingConnP2PLinks().size() > state.getMaxNumNoneP2PConnections()) {
            int numClientsToSend = state.getMaxNumNoneP2PConnections() / 2;
            int numSwaps = Math.min(Math.min(numClientsToSend, maxClientsCanReceive), state.getNotAcceptingConnP2PLinks().size());
            for (int i = 0; i < numSwaps; i++) {
                // Send a DOSWAP message to numSwaps clients
                List<String> nonP2PLinkUIDs = new ArrayList<>(state.getNotAcceptingConnP2PLinks().keySet());
                P2PDoSwap doSwapMg = new P2PDoSwap(MiniData.getRandomData(8), address, nonP2PLinkUIDs.get(i));
                retMessages.add(new Message(P2PManager.P2P_SEND_MSG).addString("uid", nonP2PLinkUIDs.get(i)).addObject("json", doSwapMg.toJson()));
            }
        }
        return retMessages;
    }

}
