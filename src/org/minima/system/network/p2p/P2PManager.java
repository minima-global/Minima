package org.minima.system.network.p2p;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import org.minima.database.MinimaDB;
import org.minima.objects.base.MiniData;
import org.minima.system.network.minima.NIOClient;
import org.minima.system.network.minima.NIOClientInfo;
import org.minima.system.network.p2p.messages.P2PDoSwap;
import org.minima.system.network.p2p.messages.P2PGreeting;
import org.minima.system.network.p2p.messages.P2PWalkLinks;
import org.minima.system.network.p2p.params.P2PParams;
import org.minima.system.network.p2p.params.P2PTestParams;
import org.minima.system.params.GeneralParams;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.RPCClient;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;

public class P2PManager extends MessageProcessor {

    /**
     * Reset functions - for SSH Tunnel / Maxima
     */
    public static final String P2P_RESET = "P2P_RESET";

    /**
     * A loop message repeated every so often
     */
    public static final String P2P_LOOP = "P2P_LOOP";
    public static final String P2P_ASSESS_CONNECTIVITY = "P2P_ASSESS_CONNECTIVITY";
    public static final String P2P_UPDATE_HASH_RATE = "P2P_UPDATE_HASH_RATE";
    public static final String P2P_SEND_MSG = "P2P_SEND_MSG";
    public static final String P2P_SEND_MSG_TO_ALL = "P2P_SEND_MSG_TO_ALL";
    public static final String P2P_SEND_CONNECT = "P2P_SEND_CONNECT";
    public static final String P2P_SEND_DISCONNECT = "P2P_SEND_DISCONNECT";
    public static final String P2P_METRICS = "P2P_METRICS";

    public static final String P2P_SAVE_DATA = "P2P_SAVE_DATA";

    public static final String ADDRESS_LITERAL = "address";

    private static final Random rand = new Random();
    private final P2PState state = new P2PState();

    public P2PManager() {
        super("P2PMANAGER");

        if (GeneralParams.TEST_PARAMS) {
            MinimaLogger.log("[+] P2P System using Test Params");
            P2PTestParams.setTestParams();
        }
        //And start the loop timer..
        PostTimerMessage(new TimerMessage(10_000, P2P_LOOP));
        PostTimerMessage(new TimerMessage(P2PParams.NODE_NOT_ACCEPTING_CHECK_DELAY, P2P_ASSESS_CONNECTIVITY));
        PostTimerMessage(new TimerMessage(P2PParams.SAVE_DATA_DELAY, P2P_SAVE_DATA));
        PostTimerMessage(new TimerMessage(P2PParams.HASH_RATE_UPDATE_DELAY, P2P_UPDATE_HASH_RATE));
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
                msgs.addAll(WalkLinksFuncs.onReturnedWalkMsg(state, p2pWalkLinks, state.getMaxNumP2PConnections()));
            }
        } else {
            msgs.add(WalkLinksFuncs.onWalkLinkResponseMsg(state, p2pWalkLinks));
        }
        return msgs;
    }

    protected static Message processOutgoingWalkMessage(P2PWalkLinks p2pWalkLinks, NIOClientInfo clientInfo, P2PState state) {
        Message sendMsg;
        if (p2pWalkLinks.isWalkInLinks()) {
            sendMsg = WalkLinksFuncs.onInLinkWalkMsg(state, p2pWalkLinks, clientInfo, P2PFunctions.getAllConnections());
        } else {
            sendMsg = WalkLinksFuncs.onOutLinkWalkMsg(state, p2pWalkLinks, clientInfo, state.getMaxNumP2PConnections(), P2PFunctions.getAllConnections());
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
        MinimaLogger.log("[+] P2P Version: " + P2PParams.VERSION);

        List<InetSocketAddress> peers = p2pdb.getPeersList();
        state.getKnownPeers().addAll(peers);
        state.setAcceptingInLinks(GeneralParams.IS_ACCEPTING_IN_LINKS);
        state.setMyMinimaAddress(GeneralParams.MINIMA_HOST);

        if (GeneralParams.IS_HOST_SET) {
            state.setHostSet(true);
        }

        state.setNoConnect(GeneralParams.NOCONNECT);

        //Initialise..
        //..
        state.setMaxNumNoneP2PConnections(P2PParams.TGT_NUM_NONE_P2P_LINKS);
        if (state.isAcceptingInLinks()) {
            state.setMaxNumP2PConnections(P2PParams.TGT_NUM_LINKS);
        } else {
            state.setMaxNumP2PConnections(P2PParams.MIN_NUM_CONNECTIONS);
        }

        InetSocketAddress connectionAddress = null;
        if (!state.isNoConnect()) {
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
        NIOClient info = (NIOClient) zMessage.getObject("client");
        List<Message> msgs = SwapLinksFunctions.onConnected(state, info.isIncoming(), info);
        msgs.addAll(SwapLinksFunctions.onConnectedLoadBalanceRequest(state, P2PFunctions.getAllConnections()));
        return msgs;
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
                boolean noConnect = SwapLinksFunctions.processGreeting(state, greeting, uid, client, state.isNoConnect());
                if (!noConnect) {
                    state.setNoConnect(false);
                }
            }
            if (swapLinksMsg.containsKey("req_ip")) {
                P2PFunctions.sendP2PMessage(uid, SwapLinksFunctions.processRequestIPMsg(swapLinksMsg, P2PFunctions.getNIOCLientInfo(uid).getHost()));
            }
            if (swapLinksMsg.containsKey("res_ip")) {
                SwapLinksFunctions.processResponseIPMsg(state, swapLinksMsg);
            }
            if (swapLinksMsg.containsKey("notAcceptingMsg")) {
                state.getInLinks().remove(uid);
                state.getOutLinks().remove(uid);
                state.getNotAcceptingConnP2PLinks().remove(uid);
                state.getNoneP2PLinks().remove(uid);

                state.getNotAcceptingConnP2PLinks().put(uid, state.getAllLinks().get(uid));
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

    protected static List<Message> assessConnectivity(P2PState state) {
        List<Message> sendmsgs = new ArrayList<>();
        if (state.getInLinks().isEmpty() && state.getNotAcceptingConnP2PLinks().isEmpty() && state.getNoneP2PLinks().isEmpty() && !state.getOutLinks().isEmpty()) {
            state.setAcceptingInLinks(false);
            JSONObject notAcceptingMsg = new JSONObject();
            notAcceptingMsg.put("notAcceptingMsg", false);
            sendmsgs.add(new Message(P2PManager.P2P_SEND_MSG_TO_ALL).addObject("json", notAcceptingMsg));
        }
        return sendmsgs;
    }

    private List<Message> processLoop(P2PState state) {
        List<Message> sendMsgs = new ArrayList<>();
        if (state.getOutLinks().size() >= state.getMaxNumP2PConnections()) {
            state.setLoopDelay(P2PParams.LOOP_DELAY + (long) rand.nextInt(P2PParams.LOOP_DELAY_VARIABILITY));
        } else {
            state.setLoopDelay(10_000 + (long) rand.nextInt(3_000));
        }
        state.getKnownPeers().remove(state.getMyMinimaAddress());

        if (!state.isNoConnect()) {

            if (!state.getKnownPeers().isEmpty()) {
                if (state.isDoingDiscoveryConnection()) {
                    // Loop is set to be quite fast at this point to ensure we connect to the network
                    InetSocketAddress connectionAddress = (InetSocketAddress) state.getKnownPeers().toArray()[rand.nextInt(state.getKnownPeers().size())];
                    P2PFunctions.checkConnect(connectionAddress.getHostString(), connectionAddress.getPort());
                } else if (state.getOutLinks().size() < state.getMaxNumP2PConnections()) {
                    InetSocketAddress connectionAddress = (InetSocketAddress) state.getKnownPeers().toArray()[rand.nextInt(state.getKnownPeers().size())];
                    P2PFunctions.checkConnect(connectionAddress.getHostString(), connectionAddress.getPort());
                } else if (state.isAcceptingInLinks()) {
                    sendMsgs.addAll(SwapLinksFunctions.joinScaleOutLinks(state, state.getMaxNumP2PConnections(), P2PFunctions.getAllConnections()));
                    sendMsgs.addAll(SwapLinksFunctions.requestInLinks(state, state.getMaxNumP2PConnections(), P2PFunctions.getAllConnections()));
                    sendMsgs.addAll(SwapLinksFunctions.onConnectedLoadBalanceRequest(state, P2PFunctions.getAllConnections()));
                }

            } else {
                MinimaLogger.log("[-] WARNING : No Known peers ( -clean + delay )");
                state.setDoingDiscoveryConnection(true);
                InetSocketAddress connectionAddress = P2PParams.DEFAULT_NODE_LIST.get(rand.nextInt(P2PParams.DEFAULT_NODE_LIST.size()));
                MinimaLogger.log("[+] Doing discovery connection with default node: " + connectionAddress);
                if (connectionAddress != null) {
                    sendMsgs.add(new Message(P2PManager.P2P_SEND_CONNECT)
                            .addObject(ADDRESS_LITERAL, connectionAddress));

                }
            }
        }
//        JSONObject status = getStatus();
//        status.remove("p2p_state");
//        MinimaLogger.log(status.toString());
        return sendMsgs;
    }

    public JSONObject getStatus(boolean fullDetails) {
        int numInbound = 0;
        int numOutbound = 0;
        for (NIOClientInfo info : P2PFunctions.getAllConnections()) {
            if (info.isConnected()) {
                if (info.isIncoming()) {
                    numInbound += 1;
                } else {
                    numOutbound += 1;
                }
            }
        }

        JSONObject ret = new JSONObject();
        ret.put("deviceHashRate", state.getDeviceHashRate());
        ret.put("address", state.getMyMinimaAddress().toString().replace("/", ""));
        ret.put("isAcceptingInLinks", state.isAcceptingInLinks());
        ret.put("numInLinks", state.getInLinks().size());
        ret.put("numOutLinks", state.getOutLinks().size());
        ret.put("numNotAcceptingConnP2PLinks", state.getNotAcceptingConnP2PLinks().size());
        ret.put("numNoneP2PLinks", state.getNoneP2PLinks().size());
        ret.put("numKnownPeers", state.getKnownPeers().size());
        ret.put("numAllLinks", state.getAllLinks().size());
        ret.put("nio_inbound", numInbound);
        ret.put("nio_outbound", numOutbound);
        if (fullDetails && state.getMyMinimaAddress() != null && state.isAcceptingInLinks()) {
            ret.put("p2p_state", state.toJson());
        }

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

    @Override
    protected void processMessage(Message zMessage) throws Exception {
        List<Message> sendMsgs = new ArrayList<>();
        if (zMessage.isMessageType(P2PFunctions.P2P_INIT)) {
            sendMsgs.addAll(init(state));
            PostTimerMessage(new TimerMessage(P2PParams.METRICS_DELAY, P2P_METRICS));
        } else if (zMessage.isMessageType(P2PFunctions.P2P_SHUTDOWN)) {
            shutdown();
        } else if (zMessage.isMessageType(P2P_SAVE_DATA)) {
            P2PDB p2pdb = MinimaDB.getDB().getP2PDB();
            p2pdb.setPeersList(new ArrayList<>(state.getKnownPeers()));
            PostTimerMessage(new TimerMessage(P2PParams.SAVE_DATA_DELAY, P2P_SAVE_DATA));
        } else if (zMessage.isMessageType(P2PFunctions.P2P_CONNECTED)) {
            String uid = zMessage.getString("uid");
            NIOClient client = (NIOClient) zMessage.getObject("client");
            state.getAllLinks().put(uid, new InetSocketAddress(client.getHost(), client.getPort()));
            sendMsgs.addAll(connect(zMessage, state));
        } else if (zMessage.isMessageType(P2PFunctions.P2P_DISCONNECTED)) {
            String uid = zMessage.getString("uid");
            state.getAllLinks().remove(uid);
            SwapLinksFunctions.onDisconnected(state, zMessage);
        } else if (zMessage.isMessageType(P2PFunctions.P2P_MESSAGE)) {
            sendMsgs.addAll(processJsonMessages(zMessage, state));
        } else if (zMessage.isMessageType(P2P_LOOP)) {
            sendMsgs.addAll(processLoop(state));
            PostTimerMessage(new TimerMessage(state.getLoopDelay(), P2P_LOOP));

        } else if (zMessage.isMessageType(P2P_RESET)) {
            MinimaLogger.log("[+] P2P Reset in process");
            state.setAcceptingInLinks(GeneralParams.IS_ACCEPTING_IN_LINKS);
            state.setMyMinimaAddress(GeneralParams.MINIMA_HOST);
            state.setHostSet(GeneralParams.IS_HOST_SET);

            //Same as Loop but no timer message
            sendMsgs.addAll(processLoop(state));

        } else if (zMessage.isMessageType(P2PFunctions.P2P_NOCONNECT)) {
            NIOClient client = (NIOClient) zMessage.getObject("client");
            InetSocketAddress conn = new InetSocketAddress(client.getHost(), client.getPort());
            state.getKnownPeers().remove(conn);
        } else if (zMessage.isMessageType(P2P_ASSESS_CONNECTIVITY)) {
            sendMsgs.addAll(assessConnectivity(state));
            PostTimerMessage(new TimerMessage(P2PParams.NODE_NOT_ACCEPTING_CHECK_DELAY, P2P_ASSESS_CONNECTIVITY));
        } else if (zMessage.isMessageType(P2P_UPDATE_HASH_RATE)) {
            final int hashes = 1000000;
            long timestart = System.currentTimeMillis();

            MiniData data = MiniData.getRandomData(32);
            for (int i = 0; i < hashes; i++) {
                data = Crypto.getInstance().hashObject(data);
            }

            long timediff = System.currentTimeMillis() - timestart;

            float speed = (hashes * 1000) / timediff;
            float megspeed = speed / 1000000;

            state.setDeviceHashRate(megspeed);
            PostTimerMessage(new TimerMessage(P2PParams.HASH_RATE_UPDATE_DELAY, P2P_UPDATE_HASH_RATE));
        } else if (zMessage.isMessageType(P2P_METRICS)) {
            PostTimerMessage(new TimerMessage(P2PParams.METRICS_DELAY, P2P_METRICS));
            JSONObject data = state.toJson();
            int numInbound = 0;
            int numOutbound = 0;
            for (NIOClientInfo info : P2PFunctions.getAllConnections()) {
                if (info.isConnected()) {
                    if (info.isIncoming()) {
                        numInbound += 1;
                    } else {
                        numOutbound += 1;
                    }
                }
            }
            data.put("nio_inbound", numInbound);
            data.put("nio_outbound", numOutbound);
            RPCClient.sendPOST(P2PParams.METRICS_URL, data.toString(), "application/json");
        }
        sendMessages(sendMsgs);
    }

    private void sendMessages(List<Message> sendMessages) throws IOException {
        if (!sendMessages.isEmpty()) {
            for (Message msg : sendMessages) {
                if (msg != null) {
                    if (msg.isMessageType(P2P_SEND_CONNECT)) {
                        InetSocketAddress address = (InetSocketAddress) msg.getObject(ADDRESS_LITERAL);
                        P2PFunctions.checkConnect(address.getHostString(), address.getPort());
                    } else if (msg.isMessageType(P2P_SEND_DISCONNECT)) {
                        String uid = msg.getString("uid");
                        P2PFunctions.disconnect(uid);
                    } else if (msg.isMessageType(P2P_SEND_MSG)) {
                        P2PFunctions.sendP2PMessage(msg.getString("uid"), SwapLinksFunctions.wrapP2PMsg((JSONObject) msg.getObject("json")));
                    } else if (msg.isMessageType(P2P_SEND_MSG_TO_ALL)) {
                        P2PFunctions.sendP2PMessageAll(SwapLinksFunctions.wrapP2PMsg((JSONObject) msg.getObject("json")));
                    }
                }
            }
        }
    }
}
