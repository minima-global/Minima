package org.minima.system.network.p2p;

import org.minima.database.MinimaDB;
import org.minima.system.network.minima.NIOClientInfo;
import org.minima.system.network.p2p.messages.P2PDoSwap;
import org.minima.system.network.p2p.messages.P2PGreeting;
import org.minima.system.network.p2p.messages.P2PWalkLinks;
import org.minima.system.network.p2p.params.P2PParams;
import org.minima.system.network.p2p.params.P2PTestParams;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class P2PManager extends MessageProcessor {

    /**
     * A loop message repeated every so often
     */
    public static final String P2P_LOOP = "P2P_LOOP";
    public static final String P2P_ASSESS_CONNECTIVITY = "P2P_ASSESS_CONNECTIVITY";

    public static final String P2P_SEND_MSG = "P2P_SEND_MSG";
    public static final String P2P_SEND_MSG_TO_ALL = "P2P_SEND_MSG_TO_ALL";
    public static final String P2P_SEND_CONNECT = "P2P_CONNECT";
    public static final String P2P_SEND_DISCONNECT = "P2P_DISCONNECT";

    private final P2PState state = new P2PState();

    private static final Random rand = new Random();

    public static final String ADDRESS_LITERAL = "address";

    public P2PManager() {
        super("P2PMANAGER");

        if (GeneralParams.TEST_PARAMS) {
            MinimaLogger.log("[+] P2P System using Test Params");
            P2PTestParams.setTestParams();
        } else {
            MinimaLogger.log("[+] P2P System NOT using Test Params");
        }
        //And start the loop timer..
        PostTimerMessage(new TimerMessage(P2PParams.LOOP_DELAY, P2P_LOOP));
        PostTimerMessage(new TimerMessage(P2PParams.NODE_NOT_ACCEPTING_CHECK_DELAY, P2P_ASSESS_CONNECTIVITY));
    }

    protected static List<Message> processWalkLinksMsg(JSONObject zMessage, NIOClientInfo clientInfo, P2PState state) {
        P2PWalkLinks p2pWalkLinks = P2PWalkLinks.readFromJSON(zMessage);
        List<Message> sendMsg = new ArrayList<>();
        if (p2pWalkLinks.isReturning()) {
            sendMsg.addAll(processReturningMessage(p2pWalkLinks, state));
        } else {
            sendMsg.add(processOutgoingWalkMessage(p2pWalkLinks, clientInfo, state));
        }
        return sendMsg;
    }

    protected static List<Message> processReturningMessage(P2PWalkLinks p2pWalkLinks, P2PState state) {
        List<Message> msgs = new ArrayList<>();
        if (state.getMyMinimaAddress().equals(p2pWalkLinks.getPathTaken().get(0))) {
            if (p2pWalkLinks.isClientWalk()) {
                msgs.addAll(WalkLinksFuncs.onReturnedLoadBalanceWalkMsg(state, p2pWalkLinks));
            } else {
                msgs.addAll(WalkLinksFuncs.onReturnedWalkMsg(state, p2pWalkLinks, P2PParams.TGT_NUM_LINKS));
            }
        } else {
            msgs.add(WalkLinksFuncs.onWalkLinkResponseMsg(state, p2pWalkLinks, P2PFunctions.getAllConnections()));
        }
        return msgs;
    }

    protected static Message processOutgoingWalkMessage(P2PWalkLinks p2pWalkLinks, NIOClientInfo clientInfo, P2PState state) {
        Message sendMsg;
        if (p2pWalkLinks.isWalkInLinks()) {
            sendMsg = WalkLinksFuncs.onInLinkWalkMsg(state, p2pWalkLinks, clientInfo, P2PFunctions.getAllConnections());
        } else {
            sendMsg = WalkLinksFuncs.onOutLinkWalkMsg(state, p2pWalkLinks, clientInfo, P2PParams.TGT_NUM_LINKS, P2PFunctions.getAllConnections());
        }
        return sendMsg;
    }

    protected static List<Message> init(P2PState state) {
        List<Message> msgs = new ArrayList<>();
        //Get the P2P DB
        P2PDB p2pdb = MinimaDB.getDB().getP2PDB();
        String p2pVersion = p2pdb.getVersion();
        if (!p2pVersion.split("\\.")[0].equals(P2PParams.VERSION.split("\\.")[0])) {
            MinimaLogger.log("[-] P2P DB is not compatible with this P2P version. P2P DB Version: " + p2pVersion + " Running P2P Version: " + P2PParams.VERSION);
            p2pdb.setVersion();
            p2pdb.setPeersList(new ArrayList<>());
        }

        List<InetSocketAddress> peers = p2pdb.getPeersList();
        state.getKnownPeers().addAll(peers);

        //Initialise..
        //..
        state.setMaxNumNoneP2PConnections(P2PParams.TGT_NUM_NONE_P2P_LINKS);

        InetSocketAddress connectionAddress = null;
        if (!GeneralParams.NOCONNECT) {
            if (!GeneralParams.P2P_ROOTNODE.isEmpty()) {
                String host = GeneralParams.P2P_ROOTNODE.split(":")[0];
                int port = Integer.parseInt(GeneralParams.P2P_ROOTNODE.split(":")[1]);
                connectionAddress = new InetSocketAddress(host, port);
                state.getKnownPeers().add(connectionAddress);
                MinimaLogger.log("[+] Connecting to specified node: " + connectionAddress);
            } else if (!peers.isEmpty()) {
                connectionAddress = peers.get(rand.nextInt(peers.size()));
                MinimaLogger.log("[+] Connecting to saved node: " + connectionAddress);
            } else {
                state.setDoingDiscoveryConnection(true);
				connectionAddress = P2PParams.DEFAULT_NODE_LIST.get(rand.nextInt(P2PParams.DEFAULT_NODE_LIST.size()));
				MinimaLogger.log("[+] Doing discovery connection with default node: " + connectionAddress);
            }
        }
        if (connectionAddress != null) {
            msgs.add(new Message(P2PManager.P2P_SEND_CONNECT)
                    .addObject(ADDRESS_LITERAL, connectionAddress));
        }
        return msgs;
    }

    public static List<Message> connect(Message zMessage, P2PState state) {
        String uid = zMessage.getString("uid");
        boolean incoming = zMessage.getBoolean("incoming");
        NIOClientInfo info = P2PFunctions.getNIOCLientInfo(uid);
        List<Message> msgs = SwapLinksFunctions.onConnected(state, uid, incoming, info);
        msgs.addAll(SwapLinksFunctions.onConnectedLoadBalanceRequest(state, P2PFunctions.getAllConnections()));
        return msgs;
    }

    public JSONObject getStatus() {
        JSONObject ret = new JSONObject();

        ret.put("isAcceptingInLinks", state.isAcceptingInLinks());
        ret.put("numInLinks", state.getInLinks().size());
        ret.put("numOutLinks", state.getOutLinks().size());
        ret.put("numNotAcceptingConnP2PLinks", state.getNotAcceptingConnP2PLinks().size());
        ret.put("numNoneP2PLinks", state.getNoneP2PLinks().size());
        ret.put("numKnownPeers", state.getKnownPeers().size());

        return ret;
    }

    public void shutdown() {
        //Write stuff to P2P DB..
        P2PDB p2pdb = MinimaDB.getDB().getP2PDB();
        p2pdb.setVersion();
        p2pdb.setPeersList(new ArrayList<>(state.getKnownPeers()));

        //I save the DB.. you don't do it..!

        //And finish with..
        stopMessageProcessor();
    }

    protected static List<Message> processJsonMessages(Message zMessage, P2PState state) throws IOException {
        //Get the message..
        List<Message> sendMsgs = new ArrayList<>();
        JSONObject message = (JSONObject) zMessage.getObject("message");
        String uid = zMessage.getString("uid");
        NIOClientInfo client = P2PFunctions.getNIOCLientInfo(uid);

        JSONObject swapLinksMsg = (JSONObject) message.get("swap_links_p2p");
        if (swapLinksMsg != null) {
            if (swapLinksMsg.containsKey("greeting")) {
                P2PGreeting greeting = P2PGreeting.fromJSON((JSONObject) swapLinksMsg.get("greeting"));
                SwapLinksFunctions.updateKnownPeersFromGreeting(state, greeting);
                boolean noconnect = SwapLinksFunctions.processGreeting(state, greeting, uid, client, GeneralParams.NOCONNECT);
                if (GeneralParams.NOCONNECT != noconnect) {
                    GeneralParams.NOCONNECT = noconnect;
                }
            }
            if (swapLinksMsg.containsKey("req_ip")) {
                P2PFunctions.sendP2PMessage(uid, SwapLinksFunctions.processRequestIPMsg(swapLinksMsg, P2PFunctions.getNIOCLientInfo(uid).getHost()));
            }
            if (swapLinksMsg.containsKey("res_ip")) {
                SwapLinksFunctions.processResponseIPMsg(state, swapLinksMsg);
            }
            if (swapLinksMsg.containsKey("notAcceptingMsg")) {
                state.getNotAcceptingConnP2PLinks().put(uid, state.getInLinks().remove(uid));
            }
            if (swapLinksMsg.containsKey("walk_links")) {
                sendMsgs.addAll(processWalkLinksMsg(swapLinksMsg, client, state));
            }
            if (swapLinksMsg.containsKey("do_swap")) {
                // Execute Do Swap
                P2PDoSwap doSwap = P2PDoSwap.readFromJson(swapLinksMsg);
                sendMsgs.add(new Message(P2P_SEND_CONNECT).addObject(ADDRESS_LITERAL, doSwap.getSwapTarget()));
                sendMsgs.add(new Message(P2P_SEND_DISCONNECT).addString("uid", uid));
            }
        }
        return sendMsgs;
    }

    protected static List<Message> assessConnectivity(P2PState state){
        List<Message> sendmsgs = new ArrayList<>();
        if (!state.getInLinks().isEmpty() && !state.getOutLinks().isEmpty()) {
            state.setAcceptingInLinks(false);
            JSONObject notAcceptingMsg = new JSONObject();
            notAcceptingMsg.put("notAcceptingMsg", false);
            sendmsgs.add(new Message(P2PManager.P2P_SEND_MSG_TO_ALL).addObject("json", notAcceptingMsg));

        }
        return sendmsgs;
    }

    @Override
    protected void processMessage(Message zMessage) throws Exception {
        List<Message> sendMsgs = new ArrayList<>();
        if (zMessage.isMessageType(P2PFunctions.P2P_INIT)) {
            sendMsgs.addAll(init(state));
        } else if (zMessage.isMessageType(P2PFunctions.P2P_SHUTDOWN)) {
            shutdown();
        } else if (zMessage.isMessageType(P2PFunctions.P2P_CONNECTED)) {
            sendMsgs.addAll(connect(zMessage, state));
        } else if (zMessage.isMessageType(P2PFunctions.P2P_DISCONNECTED)) {
            SwapLinksFunctions.onDisconnected(state, zMessage);
            MinimaLogger.log(getStatus().toString());
        } else if (zMessage.isMessageType(P2PFunctions.P2P_MESSAGE)) {
            sendMsgs.addAll(processJsonMessages(zMessage, state));
        } else if (zMessage.isMessageType(P2P_LOOP)) {
            sendMsgs.addAll(processLoop(state));
            PostTimerMessage(new TimerMessage(state.getLoopDelay(), P2P_LOOP));
        } else if (zMessage.isMessageType(P2P_ASSESS_CONNECTIVITY)) {
            sendMsgs.addAll(assessConnectivity(state));
            PostTimerMessage(new TimerMessage(P2PParams.NODE_NOT_ACCEPTING_CHECK_DELAY, P2P_ASSESS_CONNECTIVITY));
        }
        sendMessages(sendMsgs);
    }

    private void sendMessages(List<Message> sendMessages) throws IOException{
        if (!sendMessages.isEmpty()) {
            for (Message msg : sendMessages) {
                if (msg != null) {
                    if (msg.isMessageType(P2P_SEND_CONNECT)) {
                        InetSocketAddress address = (InetSocketAddress) msg.getObject(ADDRESS_LITERAL);
                        P2PFunctions.connect(address.getHostString(), address.getPort());
                    } else if (msg.isMessageType(P2P_SEND_DISCONNECT)) {
                        String uid = msg.getString("uid");
                        P2PFunctions.disconnect(uid);
                    }else if (msg.isMessageType(P2P_SEND_MSG)) {
                        P2PFunctions.sendP2PMessage(msg.getString("uid"), SwapLinksFunctions.wrapP2PMsg((JSONObject) msg.getObject("json")));
                    } else if (msg.isMessageType(P2P_SEND_MSG_TO_ALL)) {
                        P2PFunctions.sendP2PMessageAll(SwapLinksFunctions.wrapP2PMsg((JSONObject) msg.getObject("json")));
                    }
                }
            }
        }
    }

    private static List<Message> processLoop(P2PState state) throws IOException {
        List<Message> sendMsgs = new ArrayList<>();
        if (!GeneralParams.NOCONNECT) {
            state.setLoopDelay(P2PParams.LOOP_DELAY + (long) rand.nextInt(P2PParams.LOOP_DELAY_VARIABILITY));
            int numEntryNodes = 1;
            if (!state.isAcceptingInLinks()) {
                numEntryNodes = P2PParams.MIN_NUM_CONNECTIONS;
            }
            if (state.isDoingDiscoveryConnection()) {
                // Loop is set to be quite fast at this point to ensure we connect to the network
                state.setLoopDelay(5_000 + (long) rand.nextInt(3_000));
                if (!state.getKnownPeers().isEmpty()) {
                    InetSocketAddress connectionAddress = (InetSocketAddress) state.getKnownPeers().toArray()[rand.nextInt(state.getKnownPeers().size())];
                    P2PFunctions.connect(connectionAddress.getHostString(), connectionAddress.getPort());
                }
            } else if (state.getOutLinks().size() < numEntryNodes) {
                if (!state.getKnownPeers().isEmpty()) {
                    InetSocketAddress connectionAddress = (InetSocketAddress) state.getKnownPeers().toArray()[rand.nextInt(state.getKnownPeers().size())];
                    P2PFunctions.connect(connectionAddress.getHostString(), connectionAddress.getPort());
                }
                state.setLoopDelay(10_000 + (long) rand.nextInt(3_000));
            }

            sendMsgs.addAll(SwapLinksFunctions.joinScaleOutLinks(state, P2PParams.TGT_NUM_LINKS, P2PFunctions.getAllConnections()));
            sendMsgs.addAll(SwapLinksFunctions.requestInLinks(state, P2PParams.TGT_NUM_LINKS, P2PFunctions.getAllConnections()));
            sendMsgs.addAll(SwapLinksFunctions.onConnectedLoadBalanceRequest(state, P2PFunctions.getAllConnections()));
        }
        return sendMsgs;
    }
}
