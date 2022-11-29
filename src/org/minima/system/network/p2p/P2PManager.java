package org.minima.system.network.p2p;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Random;

import org.minima.database.MinimaDB;
import org.minima.objects.Greeting;
import org.minima.system.network.minima.NIOClient;
import org.minima.system.network.minima.NIOClientInfo;
import org.minima.system.network.minima.NIOManager;
import org.minima.system.network.p2p.messages.InetSocketAddressIO;
import org.minima.system.network.p2p.messages.P2PDoSwap;
import org.minima.system.network.p2p.messages.P2PGreeting;
import org.minima.system.network.p2p.messages.P2PWalkLinks;
import org.minima.system.network.p2p.params.P2PParams;
import org.minima.system.network.p2p.params.P2PTestParams;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;

public class P2PManager extends MessageProcessor {

    /**
     * Reset functions - for SSH Tunnel / Maxima
     */
    public static final String P2P_RESET 			= "P2P_RESET";
    public static final String P2P_RANDOM_CONNECT 	= "P2P_RANDOM_CONNECT";

    /**
     * A loop message repeated every so often
     */
    public static final String P2P_LOOP = "P2P_LOOP";
    public static final String P2P_ASSESS_CONNECTIVITY = "P2P_ASSESS_CONNECTIVITY";
    public static final String P2P_SEND_MSG = "P2P_SEND_MSG";
    public static final String P2P_SEND_MSG_TO_ALL = "P2P_SEND_MSG_TO_ALL";
    public static final String P2P_SEND_CONNECT = "P2P_SEND_CONNECT";
    public static final String P2P_SEND_DISCONNECT = "P2P_SEND_DISCONNECT";

    public static final String P2P_ADD_PEER = "P2P_ADD_PEER";
    public static final String P2P_REMOVE_PEER = "P2P_REMOVE_PEER";
    public static final String P2P_SAVE_DATA = "P2P_SAVE_DATA";

    public static final String ADDRESS_LITERAL = "address";

    private static final Random rand = new Random();
    private final P2PState state = new P2PState();

    /**
     * Separate thread for checking peers
     */
    P2PPeersChecker mPeersChecker;
    
    public P2PManager() {
        super("P2PMANAGER");
        
        if (GeneralParams.TEST_PARAMS) {
            MinimaLogger.log("[+] P2P System using Test Params");
            P2PTestParams.setTestParams();
        }

        //Start the Peers checker..
        mPeersChecker = new P2PPeersChecker(this);
        
        //And start the loop timer..
        PostTimerMessage(new TimerMessage(10_000, P2P_LOOP));
        PostTimerMessage(new TimerMessage(P2PParams.NODE_NOT_ACCEPTING_CHECK_DELAY, P2P_ASSESS_CONNECTIVITY));
        PostTimerMessage(new TimerMessage(P2PParams.SAVE_DATA_DELAY, P2P_SAVE_DATA));
    }
    
    public P2PPeersChecker getPeersChecker() {
    	return mPeersChecker;
    }
    
    public ArrayList<InetSocketAddress> getPeersCopy(){
        return state.getKnownPeersCopy();
    }

    public float getClients() {
        // Divided by number of connections clients haves to convert client connections into num clients
        return (float) state.getNoneP2PLinks().size() / P2PParams.MIN_NUM_CONNECTIONS;
    }

    protected List<Message> init(P2PState state) {
        List<Message> msgs = new ArrayList<>();
        //Get the P2P DB
        P2PDB p2pdb = MinimaDB.getDB().getP2PDB();
        String p2pVersion = p2pdb.getVersion();
        if (!p2pVersion.split("\\.")[0].equals(P2PParams.VERSION.split("\\.")[0])) {
            P2PFunctions.log_info("[-] P2P DB is not compatible with this P2P version. P2P DB Version: " + p2pVersion + " Running P2P Version: " + P2PParams.VERSION);
            p2pdb.setVersion();
            p2pdb.setPeersList(new ArrayList<>());
        }
        MinimaLogger.log("[+] P2P Version: " + P2PParams.VERSION);

        List<InetSocketAddress> peers = p2pdb.getPeersList();
        for(InetSocketAddress peer: peers){
            mPeersChecker.PostMessage(new Message(P2PPeersChecker.PEERS_ADDPEERS).addObject("address", peer));
        }
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
                mPeersChecker.PostMessage(new Message(P2PPeersChecker.PEERS_ADDPEERS).addObject("address", connectionAddress));
                P2PFunctions.log_info("[+] Connecting to specified node: " + connectionAddress);
            } else if (!peers.isEmpty()) {
                connectionAddress = peers.get(rand.nextInt(peers.size()));
                P2PFunctions.log_info("[+] Connecting to saved node: " + connectionAddress);
            } else {
                state.setDoingDiscoveryConnection(true);
                P2PFunctions.log_info("[+] Doing discovery connection with default node");
                doDiscoveryPing();
            }
        }
        if (connectionAddress != null) {
            msgs.add(new Message(P2PManager.P2P_SEND_CONNECT).addObject(ADDRESS_LITERAL, connectionAddress));
        }

        return msgs;
    }


    private void doDiscoveryPing(){
        
    	//Check how many nodes there are..
    	if(P2PParams.DEFAULT_NODE_LIST.size()==0) {
    		MinimaLogger.log("There are NO DEFAULT PEERS - please use command 'peers' to add a valid peer..");
    		return;
    	}
    	
    	while (state.isDoingDiscoveryConnection() && isRunning()){
            InetSocketAddress address = P2PParams.DEFAULT_NODE_LIST.get(rand.nextInt(P2PParams.DEFAULT_NODE_LIST.size()));
            Greeting greet = NIOManager.sendPingMessage(address.getHostString(), address.getPort(), true);
            if (greet != null) {
                JSONArray peersArrayList = (JSONArray) greet.getExtraData().get("peers-list");
                if (peersArrayList != null){
//                    MinimaLogger.log(peersArrayList.toString());
                    List<InetSocketAddress> peers = InetSocketAddressIO.addressesJSONArrayToList(peersArrayList);
                    Collections.shuffle(peers);
                    state.getKnownPeers().addAll(peers);
                    mPeersChecker.getVerifiedPeers().addAll(peers);
                    P2PFunctions.log_info("[+] Discovery Completed");
                    state.setDoingDiscoveryConnection(false);
                }
            } else {
                try {
                    Thread.sleep(5000);
                } catch (Exception ex){
                    P2PFunctions.log_debug("Wait interrupted");
                }
            }
        }
    }
    @Override
    protected void processMessage(Message zMessage) throws Exception {
    	
        List<Message> sendMsgs = new ArrayList<>();
        if (zMessage.isMessageType(P2PFunctions.P2P_INIT)) {
            sendMsgs.addAll(init(state));
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
            if (state.getOutLinks().containsKey(uid)) {
                P2PFunctions.log_debug("[-] P2P_DISCONNECTED from: " + uid + " Current outlinks: " + state.getOutLinks().size());
            }
        } else if (zMessage.isMessageType(P2PFunctions.P2P_MESSAGE)) {
            sendMsgs.addAll(processJsonMessages(zMessage, state));
        } else if (zMessage.isMessageType(P2P_LOOP)) {
            sendMsgs.addAll(processLoop(state));
            PostTimerMessage(new TimerMessage(state.getLoopDelay(), P2P_LOOP));
            
        } else if (zMessage.isMessageType(P2P_RESET)) {
            P2PFunctions.log_debug("[+] P2P Reset in process");
            state.setAcceptingInLinks(GeneralParams.IS_ACCEPTING_IN_LINKS);
            state.setMyMinimaAddress(GeneralParams.MINIMA_HOST);
            state.setHostSet(GeneralParams.IS_HOST_SET);

            //Same as Loop but no timer message
            sendMsgs.addAll(processLoop(state));

        } else if (zMessage.isMessageType(P2P_RANDOM_CONNECT)) {
            
        	//Check we have some peers
        	if(state.getKnownPeers().size()>0) {
	        	
        		//Get a Random peer..
	        	InetSocketAddress connectionAddress = (InetSocketAddress) state.getKnownPeers().toArray()[rand.nextInt(state.getKnownPeers().size())];
	            
	        	//Connect to them..
	        	sendMsgs.add(new Message(P2PManager.P2P_SEND_CONNECT).addObject(ADDRESS_LITERAL, connectionAddress));
        	}
        	
        } else if (zMessage.isMessageType(P2PFunctions.P2P_NOCONNECT)) {
            NIOClient client = (NIOClient) zMessage.getObject("client");
            InetSocketAddress conn = new InetSocketAddress(client.getHost(), client.getPort());
            state.getKnownPeers().remove(conn);
            P2PFunctions.log_debug("[-] Unable to connect to peer removing from peers list");
            List<String> uidsToRemove = new ArrayList<>();
            if (state.getInLinks().containsValue(conn)){
                for (Map.Entry<String, InetSocketAddress> entry : state.getInLinks().entrySet()) {
                    if (entry.getValue().equals(conn)){
                        uidsToRemove.add(entry.getKey());
                    }
                }
                for(String uid: uidsToRemove){
                    state.getInLinks().remove(uid);
                    state.getNotAcceptingConnP2PLinks().put(uid, state.getAllLinks().get(uid));
                }

            }
        } else if (zMessage.isMessageType(P2P_ASSESS_CONNECTIVITY)) {
            sendMsgs.addAll(assessConnectivity(state));
            PostTimerMessage(new TimerMessage(P2PParams.NODE_NOT_ACCEPTING_CHECK_DELAY, P2P_ASSESS_CONNECTIVITY));
        } else if (zMessage.isMessageType(P2P_REMOVE_PEER)) {
            InetSocketAddress address = (InetSocketAddress) zMessage.getObject("address");
            state.getKnownPeers().remove(address);
        } else if (zMessage.isMessageType(P2P_ADD_PEER)) {
            if(state.getKnownPeers().size() < 250) {
                InetSocketAddress address = (InetSocketAddress) zMessage.getObject("address");
                state.getKnownPeers().add(address);
            }
        }
        sendMessages(sendMsgs);
    }

    protected static List<Message> assessConnectivity(P2PState state) {
        List<Message> sendmsgs = new ArrayList<>();
        if (state.getInLinks().isEmpty() && state.getNotAcceptingConnP2PLinks().isEmpty() && state.getNoneP2PLinks().isEmpty() && !state.getOutLinks().isEmpty()) {
            state.setAcceptingInLinks(false);
            JSONObject notAcceptingMsg = new JSONObject();
            notAcceptingMsg.put("notAcceptingMsg", false);
            state.setMaxNumP2PConnections(P2PParams.MIN_NUM_CONNECTIONS);
            sendmsgs.add(new Message(P2PManager.P2P_SEND_MSG_TO_ALL).addObject("json", notAcceptingMsg));
        }
        return sendmsgs;
    }

    protected List<Message> processJsonMessages(Message zMessage, P2PState state) throws IOException {
        //Get the message..
        List<Message> sendMsgs = new ArrayList<>();
        JSONObject message = (JSONObject) zMessage.getObject("message");
        
        //Get the UID and CURRENT client info..
        String uid 				= zMessage.getString("uid");
        NIOClientInfo client 	= P2PFunctions.getNIOCLientInfo(uid);
        if(client == null) {
        	P2PFunctions.log_debug("[!] P2P NULL NioClient @ "+uid);
        	sendMsgs.add(new Message(P2P_SEND_DISCONNECT).addString("uid", uid));
        	return sendMsgs;
        }
        
        JSONObject swapLinksMsg = (JSONObject) message.get("swap_links_p2p");
        if (swapLinksMsg != null) {
            if (swapLinksMsg.containsKey("greeting")) {
                P2PGreeting greeting = P2PGreeting.fromJSON((JSONObject) swapLinksMsg.get("greeting"));
                for(InetSocketAddress address: greeting.getKnownPeers()){
                    mPeersChecker.PostMessage(new Message(P2PPeersChecker.PEERS_ADDPEERS).addObject("address", address));
                }
                // See if the connection the clients address has come from is a valid peer
                InetSocketAddress minimaAddress = new InetSocketAddress(client.getHost(),  greeting.getMyMinimaPort());
                mPeersChecker.PostMessage(new Message(P2PPeersChecker.PEERS_ADDPEERS).addObject("address", minimaAddress));

                boolean noConnect = SwapLinksFunctions.processGreeting(state, greeting, client, state.isNoConnect());
                if (!noConnect) {
                    state.setNoConnect(false);
                }
            }
            if (swapLinksMsg.containsKey("req_ip")) {
                P2PFunctions.sendP2PMessage(uid, SwapLinksFunctions.processRequestIPMsg(swapLinksMsg, client.getHost()));
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
                // If there are fewer connections than the min number of connections connect using the peers list
                // Min number of connections is the param clients use, so clients will always connect using the peers list
                // Then be load balanced
               if (state.getOutLinks().size() < 1) {
                    InetSocketAddress connectionAddress = (InetSocketAddress) state.getKnownPeers().toArray()[rand.nextInt(state.getKnownPeers().size())];
                    sendMsgs.add(new Message(P2PManager.P2P_SEND_CONNECT).addObject(ADDRESS_LITERAL, connectionAddress));
                } else if (state.getOutLinks().size() < state.getMaxNumP2PConnections()) {
                    sendMsgs.addAll(SwapLinksFunctions.joinScaleOutLinks(state, state.getMaxNumP2PConnections(), P2PFunctions.getAllConnections()));
                } else if (state.isAcceptingInLinks()) {
                    sendMsgs.addAll(SwapLinksFunctions.requestInLinks(state, state.getMaxNumP2PConnections(), P2PFunctions.getAllConnections()));
                    sendMsgs.addAll(SwapLinksFunctions.onConnectedLoadBalanceRequest(state, P2PFunctions.getAllConnections()));
                }

            } else {
                if (state.getAllLinks().size() == 0 && state.isStartupComplete()){
                    P2PFunctions.log_node_runner("[!] Node is not connected to the network. Attempting to join the network again now. Please check your not has an internet connection.");
                }
                if (state.getKnownPeers().size() == 0 && !state.isStartupComplete()){
                    P2PDB p2pdb = MinimaDB.getDB().getP2PDB();
                    List<InetSocketAddress> peers = p2pdb.getPeersList();
                    for(InetSocketAddress peer: peers){
                        mPeersChecker.PostMessage(new Message(P2PPeersChecker.PEERS_ADDPEERS).addObject("address", peer));
                    }
                }
                state.setDoingDiscoveryConnection(true);
                doDiscoveryPing();
            }
        }

        return sendMsgs;
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
        // If are at the node that sent the request
        if (state.getMyMinimaAddress().equals(p2pWalkLinks.getPathTaken().get(0))) {
            // if this is an in-link walk for node balancing
            if (p2pWalkLinks.isClientWalk()) {
                msgs.addAll(WalkLinksFuncs.onReturnedLoadBalanceWalkMsg(state, p2pWalkLinks));
            } else {
                // Else it's an in-link walk for scaling out-links
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

    public static List<Message> connect(Message zMessage, P2PState state) {
        NIOClient info = (NIOClient) zMessage.getObject("client");
        List<Message> msgs = SwapLinksFunctions.onConnected(state, info.isIncoming(), info);
        msgs.addAll(SwapLinksFunctions.onConnectedLoadBalanceRequest(state, P2PFunctions.getAllConnections()));
        return msgs;
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

        if (fullDetails) {
            ret.put("p2p_state", state.toJson());
            ret.put("numUnvalidatedPeers", mPeersChecker.getUnverifiedPeers().size());
        } else {
            ret.put("address", state.getMyMinimaAddress().toString().replace("/", ""));
            ret.put("isAcceptingInLinks", state.isAcceptingInLinks());
            ret.put("numInLinks", state.getInLinks().size());
            ret.put("numOutLinks", state.getOutLinks().size());
            ret.put("numNotAcceptingConnP2PLinks", state.getNotAcceptingConnP2PLinks().size());
            ret.put("numNoneP2PLinks", state.getNoneP2PLinks().size());
            ret.put("numKnownPeers", state.getKnownPeers().size());
            ret.put("numUnvalidatedPeers", mPeersChecker.getUnverifiedPeers().size());
            ret.put("numAllLinks", state.getAllLinks().size());
            ret.put("nio_inbound", numInbound);
            ret.put("nio_outbound", numOutbound);
        }

        return ret;
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

    public void shutdown() {
        //Write stuff to P2P DB..
        P2PDB p2pdb = MinimaDB.getDB().getP2PDB();
        p2pdb.setVersion();
        if (state.getKnownPeers().size() > 0){
            p2pdb.setPeersList(new ArrayList<>(state.getKnownPeers()));
        }

        //Stop the peers checker
        mPeersChecker.stopMessageProcessor();
        
        //And finish with..
        stopMessageProcessor();
    }
}
