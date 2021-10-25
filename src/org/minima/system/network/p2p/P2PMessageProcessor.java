package org.minima.system.network.p2p;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.minima.GlobalParams;
import org.minima.objects.greet.Greeting;
import org.minima.system.Main;
import org.minima.system.brains.BackupManager;
import org.minima.system.input.InputHandler;
import org.minima.system.network.NetworkHandler;
import org.minima.system.network.base.MinimaClient;
import org.minima.system.network.p2p.functions.*;
import org.minima.system.network.p2p.messages.*;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;

import java.io.File;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Random;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toList;
import static org.minima.system.network.NetworkHandler.NETWORK_CONNECT;
import static org.minima.system.network.NetworkHandler.NETWORK_DISCONNECT;

@Getter
@Slf4j(topic = "P2P")
public class P2PMessageProcessor extends MessageProcessor {

    /**
     * P2P Functions..
     */

    public static final String P2P_INIT = "P2P_INIT";
    public static final String P2P_SHUTDOWN = "P2P_SHUTDOWN";
    public static final String P2P_ON_GREETED = "P2P_ON_GREETED";
    public static final String P2P_ON_CONNECTED = "P2P_ON_CONNECTED";
    public static final String P2P_ON_DISCONNECTED = "P2P_ON_DISCONNECTED";

    public static final String P2P_LOOP = "P2P_LOOP";

    public static final String P2P_CONNECT = "P2P_CONNECT";
    public static final String P2P_DISCONNECT = "P2P_DISCONNECT";

    public static final String P2P_RENDEZVOUS = "P2P_RENDEZVOUS";
    public static final String P2P_WALK_LINKS = "P2P_WALK_LINKS";
    public static final String P2P_WALK_LINKS_RESPONSE = "P2P_WALK_LINKS_RESPONSE";
    public static final String P2P_SWAP_LINK = "P2P_SWAP_LINK";
    public static final String P2P_DO_SWAP = "P2P_DO_SWAP";
    public static final String P2P_MAP_NETWORK = "P2P_MAP_NETWORK";
    public static final String P2P_PRINT_NETWORK_MAP = "P2P_PRINT_NETWORK_MAP";
    public static final String P2P_PRINT_NETWORK_MAP_RESPONSE = "P2P_PRINT_NETWORK_MAP_RESPONSE";

    public static final String P2P_SEND_MESSAGE = "P2P_SEND_MESSAGE";
    public static final String P2P_NODE_NOT_ACCEPTING_CHECK = "P2P_NODE_NOT_ACCEPTING_CHECK";
    public static final String P2P_NODE_NOT_ACCEPTING = "P2P_NODE_NOT_ACCEPTING";
    public static final String P2P_CLIENT_UPDATE_LOOP = "P2P_CLIENT_UPDATE_LOOP";

    public static final String P2P_PRINT_EVENT_LIST = "P2P_PRINT_EVENT_LIST";


    private final P2PState state;
    private final int minimaPort;
    Message printNetworkMapRPCReq;
    //The data store
    private InetAddress hostIP;

    public P2PMessageProcessor() {
        super("P2P Message Processor");

        try {
            this.hostIP = InetAddress.getByName("localhost");
        } catch (UnknownHostException e) {
            log.error("Could not identify the local ip address: " + hostIP);
        }
        state = new P2PState(null);
        state.setAddress(new InetSocketAddress(getHostIP(), getMinimaPort()));
        this.minimaPort = 9001;

    }

    public P2PMessageProcessor(String hostIP, int minimaPort) {
        super("P2P Message Processor");

        try {
            this.hostIP = InetAddress.getByName(hostIP);
        } catch (UnknownHostException e) {
            log.error("Could not identify the local ip address: " + hostIP);
        }
        this.minimaPort = minimaPort;


        //Get the BackupManager
        BackupManager backup = Main.getMainHandler().getBackupManager();
        File p2pDataFile = backup.getBackUpFile("p2pdata.json");
        state = new P2PState(p2pDataFile);
        state.setAddress(new InetSocketAddress(getHostIP(), getMinimaPort()));
        //Start the Ball rolling..
//        this.setLOG(true);
        PostTimerMessage(new TimerMessage(1_000, P2P_LOOP));
        PostTimerMessage(new TimerMessage(GlobalParams.P2P_NODE_NOT_ACCEPTING_CHECK_DELAY, P2P_NODE_NOT_ACCEPTING_CHECK));
    }

    public void stop() {
        PostMessage(P2P_SHUTDOWN);
    }

    /**
     * You can use this to get your HOST/IP etc
     */
    protected NetworkHandler getNetworkHandler() {
        return Main.getMainHandler().getNetworkHandler();
    }

    /**
     * All current connections
     */
    protected ArrayList<MinimaClient> getCurrentMinimaClients() {
        return getNetworkHandler().getNetClients();
    }

    /**
     * All current incoming connections using the minima port
     */
    protected List<MinimaClient> getCurrentIncomingMinimaClientsOnMinimaPort() {
        return getNetworkHandler().getNetClients().stream()
                .filter(MinimaClient::isIncoming)
                .collect(toList());
    }

    /**
     * Routes messages to the correct processing function
     *
     * @param zMessage The Full Message
     */
    @Override
    protected void processMessage(Message zMessage) throws Exception {
        if (!zMessage.isMessageType(P2P_WALK_LINKS)) {
            log.debug("[+] P2PMessageProcessor processing: " + zMessage.getMessageType());
        }
        try {
            switch (zMessage.getMessageType()) {
                case P2P_SEND_MESSAGE:
                    processSendMessage(zMessage);
                    break;
                case P2P_SHUTDOWN:
                    processShutdownMsg(zMessage);
                    break;
                case P2P_ON_GREETED:
                    processOnGreetedMsg(zMessage);
                    break;
                case P2P_RENDEZVOUS:
                    processOnRendezvousMsg(zMessage);
                    break;
//                case P2P_ON_CONNECTED: NEVER USED!
//                    processOnConnectedMsg(zMessage);
//                    break;
                case P2P_ON_DISCONNECTED:
                    processOnDisconnectedMsg(zMessage);
                    break;
                case P2P_CONNECT:
                    processConnectMsg(zMessage);
                    break;
                case P2P_DISCONNECT:
                    processDisconnectMsg(zMessage);
                    break;
                case P2P_LOOP:
                    processLoopMsg(zMessage);
                    break;
                case P2P_WALK_LINKS:
                    processWalkLinksMsg(zMessage);
                    break;
                case P2P_WALK_LINKS_RESPONSE:
                    processWalkLinksResponseMsg(zMessage);
                    break;
                case P2P_SWAP_LINK:
                    processSwapLinkMsg(zMessage);
                    break;
                case P2P_DO_SWAP:
                    processDoSwapMsg(zMessage);
                    break;
                case P2P_MAP_NETWORK:
                    processNetworkMapMsg(zMessage);
                    break;
                case P2P_PRINT_NETWORK_MAP:
                    processPrintNetworkMapRequestMsg(zMessage);
                    break;
                case P2P_PRINT_NETWORK_MAP_RESPONSE:
                    processPrintNetworkMapResponseMsg(zMessage);
                    break;
                case P2P_NODE_NOT_ACCEPTING_CHECK:
                    processNodeNotAcceptingMsgCheck();
                    break;
                case P2P_NODE_NOT_ACCEPTING:
                    processNodeNotAcceptingMsg(zMessage);
                    break;
                default:
                    break;
            }
        } catch (Exception e) {

            StringBuilder builder = new StringBuilder();
            StackTraceElement[] trace = e.getStackTrace();
            for (StackTraceElement traceElement : trace)
                builder.append("\tat ").append(traceElement).append("\n");

            log.error("[!] Exception in P2P Message Processor: " + e + "\n" + builder);
        }
    }

    private void processSendMessage(Message zMessage) {
        MinimaClient client = (MinimaClient) zMessage.getObject("client");
        Message message = (Message) zMessage.getObject("message");
        client.PostMessage(message);
    }

    private void processShutdownMsg(Message zMessage) {
        log.debug("[+] P2PMessageProcessor processing P2P_SHUTDOWN message");
        // Make sure the node list is saved
        StartupFuncs.SaveNodeList(this.state);
        //And stop this Message Processor stack
        stopMessageProcessor();
    }

    private void processOnGreetedMsg(Message zMessage) {
        P2PMsgGreeting greeting = (P2PMsgGreeting) zMessage.getObject("data");
        MinimaClient client = (MinimaClient) zMessage.getObject("client");

        GreetingFuncs.onGreetedMsg(state, greeting, client, getCurrentMinimaClients()).forEach(this::PostMessage);
    }

    private void processOnRendezvousMsg(Message zMessage) {
        if (state.isRendezvousComplete()) {
            return;
        }

        P2PMsgRendezvous rendezvous = (P2PMsgRendezvous) zMessage.getObject("rendezvous");
        MinimaClient client = (MinimaClient) zMessage.getObject("client");
        StartupFuncs.processOnRendezvousMsg(state, rendezvous, client);

        PostMessage(new Message(P2PMessageProcessor.P2P_DISCONNECT)
                .addObject("client", client)
                .addInteger("attempt", 0)
                .addString("reason", "Disconnecting after sending rendezvous message")
        );
    }

    private void processConnectMsg(Message zMessage) {
        InetSocketAddress address = (InetSocketAddress) zMessage.getObject("address");
        String reason = zMessage.getString("reason");
        if (!state.getAddress().equals(address)) {

            Message msg = new Message(NETWORK_CONNECT);
            msg.addObject("address", address);
            log.debug("[+] P2P_CONNECT to: " + address + " reason: " + reason);
            Main.getMainHandler().getNetworkHandler().PostMessage(msg);
        } else {
            log.debug("[!] Attempting to connect to self");
            state.removeRandomNodeSet(address);
        }
    }


    private void processOnDisconnectedMsg(Message zMessage) {
        MinimaClient client = (MinimaClient) zMessage.getObject("client");
        if (!client.isTemp()) {
            log.debug("[!] P2P_ON_DISCONNECT Disconnected from isInLink? " + client.isIncoming() + " IP: " + client.getMinimaAddress());
//            Message walkMsg;
//            if (client.isIncoming()) {
//                walkMsg = DisconnectionFuncs.onInLinkDisconnected(state, client, getCurrentMinimaClients());
//            } else {
//                walkMsg = DisconnectionFuncs.onOutLinkDisconnected(state, client, getCurrentMinimaClients());
//            }
//            if (walkMsg != null) {
//                PostMessage(walkMsg);
//            }
            state.removeDisconnectingClient(client.getUID());
            state.removeLink(client);
            log.debug(this.state.genPrintableState());
        }

    }


    private void processDisconnectMsg(Message zMessage) {
        MinimaClient client = (MinimaClient) zMessage.getObject("client");
        String reason = zMessage.getString("reason");
        log.debug("[!] P2P_DISCONNECT Disconnecting from isInLink? " + client.isIncoming() + " IP: " + client.getMinimaAddress() + " UID: " + client.getUID() + " for: " + reason);
        int attempt = zMessage.getInteger("attempt");
        if (this.state.getOutLinks().contains(client.getMinimaAddress()) ||
                this.state.getInLinks().contains(client.getMinimaAddress()) ||
                this.state.getClientLinks().contains(client.getMinimaAddress())) {
            getNetworkHandler().PostMessage(new Message(NETWORK_DISCONNECT).addString("uid", client.getUID()));
        } else {
            if (attempt < 3) {
                TimerMessage shutdownMsg = new TimerMessage(1_000, P2P_DISCONNECT);
                shutdownMsg.addObject("client", client);
                shutdownMsg.addInteger("attempt", attempt + 1);
                shutdownMsg.addString("reason", reason + " attempt: " + attempt);
                PostTimerMessage(shutdownMsg);
            }
        }


    }

    private void processSwapLinkMsg(Message zMessage) {
        P2PMsgSwapLink swapLink = (P2PMsgSwapLink) zMessage.getObject("data");
        Message messageToSend = null;
        if (swapLink.isSwapClientReq()) {
            messageToSend = SwapFuncs.onSwapClientsReq(state, swapLink, getCurrentMinimaClients());
        } else if (swapLink.isConditionalSwapReq() && state.getInLinks().size() > GlobalParams.P2P_NUM_LINKS) {
            // Send SwapLink message if we have more inLinks than desired
            messageToSend = SwapFuncs.onSwapReq(state, swapLink, getCurrentMinimaClients());
        } else if (!swapLink.isConditionalSwapReq()) {
            messageToSend = SwapFuncs.onSwapReq(state, swapLink, getCurrentMinimaClients());
        }
        if (messageToSend != null) {
            PostMessage(messageToSend);
        }

    }

    private void processDoSwapMsg(Message zMessage) {
        P2PMsgDoSwap doSwap = (P2PMsgDoSwap) zMessage.getObject("data");
        MinimaClient client = (MinimaClient) zMessage.getObject("client");
        SwapFuncs.executeDoSwap(state, doSwap, client).forEach(this::PostMessage);
    }

    private void processWalkLinksMsg(Message zMessage) {
        P2PMsgWalkLinks msgWalkLinks = (P2PMsgWalkLinks) zMessage.getObject("data");
        Message message;
        if (msgWalkLinks.isWalkInLinks()) {
            message = WalkLinksFuncs.onInLinkWalkMsg(state, msgWalkLinks, getCurrentMinimaClients());
        } else {
            message = WalkLinksFuncs.onOutLinkWalkMsg(state, msgWalkLinks, getCurrentMinimaClients());
        }
        if (message != null) {
            PostMessage(message);
        }
    }

    public void processWalkLinksResponseMsg(Message zMessage) {
        P2PMsgWalkLinks p2pWalkLinks = (P2PMsgWalkLinks) zMessage.getObject("data");
        Message sendMsg = null;
        if (state.getAddress().equals(p2pWalkLinks.getPathTaken().get(0))) {
            log.debug("[+] P2P_WALK_LINKS_RESPONSE returned to origin node");
            if (p2pWalkLinks.isClientWalk()) {
                ArrayList<Message> msgs = WalkLinksFuncs.onReturnedClientWalkMsg(state, p2pWalkLinks);
                msgs.forEach(this::PostMessage);
            } else {
                sendMsg = WalkLinksFuncs.onReturnedWalkMsg(state, p2pWalkLinks);
            }


        } else {
            sendMsg = WalkLinksFuncs.onWalkLinkResponseMsg(state, p2pWalkLinks, getCurrentMinimaClients());
        }
        if (sendMsg != null) {
            PostMessage(sendMsg);
        }
    }

    private void processLoopMsg(Message zMessage) {
        Random rand = new Random();
        long loopDelay = GlobalParams.P2P_LOOP_DELAY + rand.nextInt(GlobalParams.P2P_LOOP_DELAY_VARIABILITY);
        int num_entry_nodes = 1;
        if (state.isClient()){
            num_entry_nodes = GlobalParams.P2P_NUM_CLIENT_CONNECTIONS;
        }
        loopDelay = 5_000 + rand.nextInt(3_000);
        if (!state.isRendezvousComplete()) {
            JoiningFuncs.joinRendezvousNode(state, getCurrentMinimaClients()).forEach(this::PostMessage);
            // Loop is set to be quite fast at this point to ensure we connect to the network
            loopDelay = 5_000 + rand.nextInt(3_000);
        } else if (state.getOutLinks().size() < 5) {
            JoiningFuncs.joinEntryNode(state, getCurrentMinimaClients()).forEach(this::PostMessage);
            // Loop is set to be quite fast at this point to ensure we connect to the network
            loopDelay = 10_000 + rand.nextInt(3_000);
        } else if (!state.isClient() && state.getOutLinks().size() < GlobalParams.P2P_NUM_LINKS) {
            JoiningFuncs.joinScaleOutLinks(state, getCurrentMinimaClients()).forEach(this::PostMessage);
        } else if (!state.isClient() && state.getInLinks().size() < GlobalParams.P2P_NUM_LINKS) {
            JoiningFuncs.requestInLinks(state, getCurrentMinimaClients()).forEach(this::PostMessage);
        }

        ArrayList<ExpiringMessage> expiringMessages = this.state.dropExpiredMessages();
//        log.debug(state.genPrintableState());

        PostTimerMessage(new TimerMessage(loopDelay, P2P_LOOP));
    }

    /**
     * Checks the the node is 'not accepting' via the number of inbound connections it currently has.
     * If it is 'not accepting' and was previously flagged as not a client {@link P2PState#isClient()}, flag as a client and broadcast 'not accepting' to outbound neighbour nodes.
     * <p/>
     * Note:  Restart of the node needed to flip back to {@link P2PState#isClient()} - true if that was the start state
     */
    private void processNodeNotAcceptingMsgCheck() {
        if (!state.isClient() && state.isRendezvousComplete()) {
            if (getCurrentIncomingMinimaClientsOnMinimaPort().size() == 0) {
                BroadcastFuncs.broadcastNodeNotAccepting(state, getCurrentMinimaClients())
                        .forEach(this::PostMessage);
                state.setClient(true);
                log.debug(state.genPrintableState());
            }
        }
        PostTimerMessage(new TimerMessage(GlobalParams.P2P_NODE_NOT_ACCEPTING_CHECK_DELAY, P2P_NODE_NOT_ACCEPTING_CHECK));
    }

    /**
     * Move broadcasting node's address from inlinks to clientlinks
     */
    private void processNodeNotAcceptingMsg(Message zMessage) {
        P2PMsgNodeNotAccepting isClientMsg = (P2PMsgNodeNotAccepting) zMessage.getObject("data");
        if (state.getInLinks().remove(isClientMsg.getBroadcaster())) {
            state.getClientLinks().add(isClientMsg.getBroadcaster());
            log.debug("[+] P2P_NODE_NOT_ACCEPTING_CHECK Moving " + isClientMsg.getBroadcaster() + " from inlinks to clientLinks");

        }
    }

    private void processNetworkMapMsg(Message zMessage) {
        // On getting a network map back
        P2PMsgNode networkMap = (P2PMsgNode) zMessage.getObject("data");
        MinimaClient client = (MinimaClient) zMessage.getObject("client");
        networkMap.setNodeAddress(client.getMinimaAddress());
        networkMap.setExpireTime(System.currentTimeMillis() + GlobalParams.P2P_NETWORK_MAP_TTL);

        state.getNetworkMap().put(networkMap.getNodeAddress(), networkMap);

        state.getActiveMappingRequests().remove(networkMap.getNodeAddress());

        ArrayList<InetSocketAddress> newAddresses = networkMap.getOutLinks().stream()
                .filter(x -> !(state.getNetworkMap().containsKey(x) || state.getActiveMappingRequests().containsKey(x)))
                .distinct()
                .collect(Collectors.toCollection(ArrayList::new));

        if (state.getNetworkMap().size() < GlobalParams.P2P_MAX_NETWORK_MAP_SIZE && !newAddresses.isEmpty()) {
            for (InetSocketAddress address : newAddresses) {
                // Connect and
                state.getConnectionDetailsMap().put(address, new ConnectionDetails(ConnectionReason.MAPPING));
                PostMessage(new Message(P2PMessageProcessor.P2P_CONNECT).addObject("address", address).addString("reason", "MAPPING connection"));
            }
        }

        if (state.getActiveMappingRequests().isEmpty()) {
            PostMessage(new Message(P2P_PRINT_NETWORK_MAP_RESPONSE));
        } else {
            log.debug("[+] P2P_MAP_NETWORK active mappings left: " + state.getActiveMappingRequests().keySet());
        }

        PostMessage(new Message(P2PMessageProcessor.P2P_DISCONNECT)
                .addObject("client", client)
                .addInteger("attempt", 0)
                .addString("reason", "Disconnecting after sending mapping message")
        );

    }

    private void processPrintNetworkMapRequestMsg(Message zMessage) {
        printNetworkMapRPCReq = zMessage;

        // Remove old network map details
        ArrayList<InetSocketAddress> keysToRemove = new ArrayList<>();
        for (InetSocketAddress key: state.getNetworkMap().keySet()){
            if (state.getNetworkMap().get(key).getExpireTime() < System.currentTimeMillis()){
                keysToRemove.add(key);
            }
        }
        for(InetSocketAddress key: keysToRemove) {
            state.getNetworkMap().remove(key);

        }

        state.getNetworkMap().put(state.getAddress(), new P2PMsgNode(state));
        ArrayList<InetSocketAddress> addresses = Stream.of(state.getRecentJoiners(), state.getOutLinks(), state.getInLinks())
                .flatMap(Collection::stream).distinct()
                .filter(x -> !(state.getNetworkMap().containsKey(x) || state.getActiveMappingRequests().containsKey(x)))
                .collect(Collectors.toCollection(ArrayList::new));

        for (InetSocketAddress address : addresses) {
            // Connect and
            state.getConnectionDetailsMap().put(address, new ConnectionDetails(ConnectionReason.MAPPING));
            PostMessage(new Message(P2PMessageProcessor.P2P_CONNECT).addObject("address", address).addString("reason", "MAPPING connection"));
        }

        PostTimerMessage(new TimerMessage(GlobalParams.P2P_MAX_NETWORK_MAP_RESPONSE_TIME, P2P_PRINT_NETWORK_MAP_RESPONSE));


    }


    private void processPrintNetworkMapResponseMsg(Message zMessage) {
        if (printNetworkMapRPCReq != null) {
            JSONObject networkMapJSON = InputHandler.getResponseJSON(printNetworkMapRPCReq);
            // nodes
            JSONArray nodes = new JSONArray();
            for (P2PMsgNode value : state.getNetworkMap().values()) {
                nodes.add(value.toDetailsJSON());
            }
            // links
            networkMapJSON.put("total_nodes", state.getNetworkMap().size());
            networkMapJSON.put("nodes", nodes);

            //All good
            InputHandler.endResponse(printNetworkMapRPCReq, true, "");

        } else {
            log.warn("[-] Failed to make network map");
        }
    }

}
