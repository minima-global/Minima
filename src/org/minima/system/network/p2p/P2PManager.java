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

    private static final P2PState state = new P2PState();

    private Random rand = new Random();

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

    public JSONObject getStatus() {
        JSONObject ret = new JSONObject();

        ret.put("isAcceptingInLinks", state.isAcceptingInLinks());
        ret.put("numInLinks", state.getInLinks().size());
        ret.put("numOutLinks", state.getOutLinks().size());
        ret.put("numNoneP2PLinks", state.getNoneP2PLinks().size());
        ret.put("numKnownPeers", state.getKnownPeers().size());

        return ret;
    }

    @Override
    protected void processMessage(Message zMessage) throws Exception {

        //For Now..
//        MinimaLogger.log(zMessage.toString());

        //Process messages..
        if (zMessage.isMessageType(P2PFunctions.P2P_INIT)) {

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
//              TODO: When we have a default Node list uncomment these lines
//				connectionAddress = P2PParams.DEFAULT_NODE_LIST.get(rand.nextInt(P2PParams.DEFAULT_NODE_LIST.size()));
//				MinimaLogger.log("[+] Doing discovery connection with default node: " + connectionAddress);
                }
            }

            if (connectionAddress != null) {
                P2PFunctions.connect(connectionAddress.getHostString(), connectionAddress.getPort());
            }

        } else if (zMessage.isMessageType(P2PFunctions.P2P_SHUTDOWN)) {

            //Write stuff to P2P DB..
            P2PDB p2pdb = MinimaDB.getDB().getP2PDB();
            p2pdb.setVersion();
            p2pdb.setPeersList(new ArrayList<>(state.getKnownPeers()));

            //I save the DB.. you don't do it..!

            //And finish with..
            stopMessageProcessor();

        } else if (zMessage.isMessageType(P2PFunctions.P2P_CONNECTED)) {
            String uid = zMessage.getString("uid");
            NIOClientInfo info = P2PFunctions.getNIOCLientInfo(uid);
            List<JSONObject> msgs = SwapLinksFunctions.onConnected(state, zMessage, info);
            for (JSONObject msg : msgs) {
                P2PFunctions.sendP2PMessage(uid, msg);
            }

        } else if (zMessage.isMessageType(P2PFunctions.P2P_DISCONNECTED)) {
            SwapLinksFunctions.onDisconnected(state, zMessage);
            MinimaLogger.log(getStatus().toString());
        } else if (zMessage.isMessageType(P2PFunctions.P2P_MESSAGE)) {
            //Get the message..
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
                    MinimaLogger.log(getStatus().toString());
                }
                if (swapLinksMsg.containsKey("req_ip")) {
                    P2PFunctions.sendP2PMessage(uid, SwapLinksFunctions.processRequestIPMsg(swapLinksMsg, P2PFunctions.getNIOCLientInfo(uid).getHost()));
                }
                if (swapLinksMsg.containsKey("res_ip")) {
                    SwapLinksFunctions.processResponseIPMsg(state, swapLinksMsg);
                }
                if (swapLinksMsg.containsKey("notAcceptingMsg")) {
                    state.getNoneP2PLinks().put(uid, state.getInLinks().remove(uid));
                }
                if (swapLinksMsg.containsKey("walk_links")) {
                    processWalkLinksMsg(swapLinksMsg, client);
                }
                if (swapLinksMsg.containsKey("do_swap")) {
                    // Execute Do Swap
                    P2PDoSwap doSwap = P2PDoSwap.readFromJson(swapLinksMsg);
                    P2PFunctions.connect(doSwap.getSwapTarget().getHostString(), doSwap.getSwapTarget().getPort());
                    P2PFunctions.disconnect(uid);
                }
            }

        } else if (zMessage.isMessageType(P2P_LOOP)) {
            processLoop();
        } else if (zMessage.isMessageType(P2P_ASSESS_CONNECTIVITY)) {
            if (!state.getInLinks().isEmpty() && !state.getOutLinks().isEmpty()) {
                state.setAcceptingInLinks(false);
                JSONObject notAcceptingMsg = new JSONObject();
                notAcceptingMsg.put("notAcceptingMsg", false);
                P2PFunctions.sendP2PMessageAll(SwapLinksFunctions.wrapP2PMsg(notAcceptingMsg));
            }
            PostTimerMessage(new TimerMessage(P2PParams.NODE_NOT_ACCEPTING_CHECK_DELAY, P2P_ASSESS_CONNECTIVITY));
        }
    }

    private void processLoop() throws IOException {
        if (!GeneralParams.NOCONNECT) {
            long loopDelay = P2PParams.LOOP_DELAY + (long) rand.nextInt(P2PParams.LOOP_DELAY_VARIABILITY);
            int numEntryNodes = 1;
            if (!state.isAcceptingInLinks()) {
                numEntryNodes = P2PParams.MIN_NUM_CONNECTIONS;
            }
            if (state.isDoingDiscoveryConnection()) {
                // Loop is set to be quite fast at this point to ensure we connect to the network
                loopDelay = 5_000 +  (long) rand.nextInt(3_000);
                if (!state.getKnownPeers().isEmpty()) {
                    InetSocketAddress connectionAddress = (InetSocketAddress) state.getKnownPeers().toArray()[rand.nextInt(state.getKnownPeers().size())];
                    P2PFunctions.connect(connectionAddress.getHostString(), connectionAddress.getPort());
                }
            } else if (state.getOutLinks().size() < numEntryNodes) {
                if (!state.getKnownPeers().isEmpty()) {
                    InetSocketAddress connectionAddress = (InetSocketAddress) state.getKnownPeers().toArray()[rand.nextInt(state.getKnownPeers().size())];
                    P2PFunctions.connect(connectionAddress.getHostString(), connectionAddress.getPort());
                }
                loopDelay = 10_000 + (long) rand.nextInt(3_000);
            }

            P2PWalkLinks walkOutLinks = SwapLinksFunctions.joinScaleOutLinks(state, P2PParams.TGT_NUM_LINKS, P2PFunctions.getAllConnections());
            if (walkOutLinks != null) {
                P2PFunctions.sendP2PMessage(walkOutLinks.getTargetUidForNextHop(), SwapLinksFunctions.wrapP2PMsg(walkOutLinks.toJson()));
            }

            P2PWalkLinks walkInLinks = SwapLinksFunctions.requestInLinks(state, P2PParams.TGT_NUM_LINKS, P2PFunctions.getAllConnections());
            if (walkInLinks != null) {
                P2PFunctions.sendP2PMessage(walkInLinks.getTargetUidForNextHop(), SwapLinksFunctions.wrapP2PMsg(walkInLinks.toJson()));
            }

            PostTimerMessage(new TimerMessage(loopDelay, P2P_LOOP));
        }

    }

    private void processWalkLinksMsg(JSONObject zMessage, NIOClientInfo clientInfo) throws IOException {
        P2PWalkLinks p2pWalkLinks = P2PWalkLinks.readFromJSON(zMessage);
        Message sendMsg = null;
        if (p2pWalkLinks.isReturning()) {
            if (state.getMyMinimaAddress().equals(p2pWalkLinks.getPathTaken().get(0))) {
//                if (p2pWalkLinks.isClientWalk()) {
//                    ArrayList<Message> msgs = WalkLinksFuncs.onReturnedClientWalkMsg(state, p2pWalkLinks);
//                    msgs.forEach(this::PostMessage);
//                } else {
                    InetSocketAddress address = WalkLinksFuncs.onReturnedWalkMsg(state, p2pWalkLinks, P2PParams.TGT_NUM_LINKS);
                    if (address != null) {
                        P2PFunctions.connect(address.getHostString(), address.getPort());
                    }
//                }
            } else {
                sendMsg = WalkLinksFuncs.onWalkLinkResponseMsg(state, p2pWalkLinks, P2PFunctions.getAllConnections());
            }
        } else {
            if (p2pWalkLinks.isWalkInLinks()) {
                sendMsg = WalkLinksFuncs.onInLinkWalkMsg(state, p2pWalkLinks, clientInfo, P2PParams.TGT_NUM_NONE_P2P_LINKS, P2PFunctions.getAllConnections());
            } else {
                sendMsg = WalkLinksFuncs.onOutLinkWalkMsg(state, p2pWalkLinks, clientInfo, P2PParams.TGT_NUM_LINKS, P2PFunctions.getAllConnections());
            }

        }
        if (sendMsg != null) {
            P2PFunctions.sendP2PMessage(sendMsg.getString("uid"), SwapLinksFunctions.wrapP2PMsg((JSONObject) sendMsg.getObject("json")));
        }
    }
}
