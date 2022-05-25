package org.minima.system.network.p2p;

import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.minima.objects.base.MiniData;
import org.minima.system.Main;
import org.minima.system.network.minima.NIOClient;
import org.minima.system.network.minima.NIOClientInfo;
import org.minima.system.network.p2p.messages.P2PGreeting;
import org.minima.system.network.p2p.messages.P2PWalkLinks;
import org.minima.system.network.p2p.params.P2PParams;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class SwapLinksFunctions {

    public static JSONObject wrapP2PMsg(JSONObject data) {
        JSONObject msg = new JSONObject();
        msg.put("swap_links_p2p", data);
        return msg;
    }

    /**
     * Adds connections to the NoneP2PLinks set whilst waiting for a valid greeting.
     * Generates and send a greeting message to the newly connected client
     * Sends a request for its IP if the node's minima address has not been set yet
     *
     * @param state    the current P2P State
     * @param incoming if the connection is an incoming one
     * @param info     NIOClientInfo for the client that just connected
     * @return a list of messages to be sent (greeting and possible a request IP message)
     */
    public static List<Message> onConnected(P2PState state,boolean incoming, NIOClient info) {
        List<Message> msgs = new ArrayList<>();
        //Get the details
        String uid = info.getUID();
        boolean sendMessages = true;
        
        if (incoming) {
            InetSocketAddress incomingAddress = new InetSocketAddress(info.getHost(), 0);
            if (state.getNoneP2PLinks().containsValue(incomingAddress)) {
                msgs.add(new Message(P2PManager.P2P_SEND_DISCONNECT).addString("uid", uid));
                sendMessages = false;
            }
            state.getNoneP2PLinks().put(uid, new InetSocketAddress(info.getHost(), 0));
        } else {
            state.getNoneP2PLinks().put(uid, new InetSocketAddress(info.getHost(), info.getPort()));
        }

        if (sendMessages) {
            P2PGreeting greeting = new P2PGreeting(state);
            msgs.add(new Message(P2PManager.P2P_SEND_MSG).addString("uid", uid).addObject("json", greeting.toJson()));

            if (!state.isHostSet()) {
                JSONObject requestIp = new JSONObject();
                MiniData secret = MiniData.getRandomData(12);
                state.setIpReqSecret(secret);
                requestIp.put("req_ip", secret.toString());
                msgs.add(new Message(P2PManager.P2P_SEND_MSG).addString("uid", uid).addObject("json", requestIp));
            }
        }
        return msgs;
    }

    /**
     * If the node has more noneP2PNodes that its desired max then
     * Start a Random Walk to find a node that can take excess clients
     *
     * @param state   P2P State
     * @param clients all NIOClients
     * @return list of load balance walk messages to send
     */
    public static List<Message> onConnectedLoadBalanceRequest(P2PState state, List<NIOClientInfo> clients) {
        List<Message> msgs = new ArrayList<>();


        if (state.isAcceptingInLinks() && state.getMyMinimaAddress() != null && state.getNotAcceptingConnP2PLinks().size() > state.getMaxNumNoneP2PConnections() && !state.getInLinks().isEmpty()) {
            InetSocketAddress nextHop = UtilFuncs.selectRandomAddress(new ArrayList<>(state.getInLinks().values()));
            NIOClientInfo minimaClient = UtilFuncs.getClientFromInetAddress(nextHop, state);
            P2PWalkLinks walkLinks = new P2PWalkLinks(true, false, minimaClient.getUID());
            walkLinks.setClientWalk(true);
            // Send out multiple load balance request messages if node is highly overloaded
            int multipleOverMax = state.getInLinks().size() / state.getMaxNumP2PConnections();
            for (int i=0; i < multipleOverMax; i++) {
                msgs.add(new Message(P2PManager.P2P_SEND_MSG).addString("uid", minimaClient.getUID()).addObject("json", walkLinks.toJson()));
            }
        }
        return msgs;
    }

    public static void onDisconnected(P2PState state, Message zMessage) {
        //Get the details
        String uid = zMessage.getString("uid");

        // Remove uid from current connections
        state.getInLinks().remove(uid);
        state.getNotAcceptingConnP2PLinks().remove(uid);
        state.getOutLinks().remove(uid);
        state.getNoneP2PLinks().remove(uid);
    }

    public static void updateKnownPeersFromGreeting(P2PState state, P2PGreeting greeting) {
        List<InetSocketAddress> newPeers = Stream.of(greeting.getInLinks(), greeting.getKnownPeers())
                .flatMap(Collection::stream)
                .distinct()
                .filter(x -> x.getPort() != 0)
                .filter(x -> !x.equals(state.getMyMinimaAddress()))
                .collect(Collectors.toCollection(ArrayList::new));

        state.getKnownPeers().addAll(newPeers);

        List<InetSocketAddress> peers = new ArrayList<>(state.getKnownPeers());
        Collections.shuffle(peers);

        // Added upto 20 peers into the list + outlinks
        int numOutLinks = state.getOutLinks().size();
        state.setKnownPeers(new HashSet<>(peers.subList(0, Math.min(peers.size(), P2PParams.PEERS_LIST_SIZE - numOutLinks))));
        state.getKnownPeers().addAll(state.getOutLinks().values());
    }

    public static boolean processGreeting(P2PState state, P2PGreeting greeting, NIOClientInfo client, boolean noconnect) {

        if (client != null) {
            String uid = client.getUID();
            String host = client.getHost();
            int port = greeting.getMyMinimaPort();
            InetSocketAddress minimaAddress = new InetSocketAddress(host, port);

            boolean addtoknown = !host.contains("127.0.0.1");
//            P2PFunctions.log_debug("P2P GREETING UID:"+uid+" valid:"+state.getNoneP2PLinks().containsKey(uid)+" @ "+minimaAddress+" addtoknown:"+addtoknown);
            state.getNoneP2PLinks().remove(uid);

            //The NIOClient has received a P2Pgreeting.. Check if NULL or Tests fail
            if(Main.getInstance() != null) {
	            NIOClient nioclient = Main.getInstance().getNIOManager().getNIOServer().getClient(uid);
                if (nioclient != null){
	                nioclient.setReceivedP2PGreeting();
                }
            }

            if (greeting.isAcceptingInLinks()) {
            	if(addtoknown) {
            		state.getKnownPeers().add(minimaAddress);
            	}

                // Peers are assumed to not be P2P Links until we get a valid P2P Greeting

                if (client.isIncoming()) {
                    state.getInLinks().put(uid, minimaAddress);
                } else {
                    state.getOutLinks().put(uid, minimaAddress);
                    if (state.getOutLinks().size() > state.getMaxNumP2PConnections()) {
                        P2PFunctions.disconnect(uid);
                    }
                }

                // Disable no connect once we get a p2p connection
                if (noconnect) {
                    noconnect = false;
                }

                if (state.isDoingDiscoveryConnection()) {
                    state.setDoingDiscoveryConnection(false);
                    P2PFunctions.disconnect(uid);
                }
            } else {
                state.getNotAcceptingConnP2PLinks().put(uid, minimaAddress);
            }
        } else {
            P2PFunctions.log_debug("[-] ERROR Client is null when processing greeting: " + greeting.toJson());
        }
        return noconnect;
    }

    public static JSONObject processRequestIPMsg(JSONObject swapLinksMsg, String host) {
        MiniData secret = new MiniData((String) swapLinksMsg.get("req_ip"));
        JSONObject responseMsg = new JSONObject();
        JSONObject IpResponse = new JSONObject();
        IpResponse.put("res_ip", host);
        IpResponse.put("secret", secret.toString());
        responseMsg.put("swap_links_p2p", IpResponse);
        return responseMsg;
    }

    public static void processResponseIPMsg(P2PState state, JSONObject swapLinksMsg) {
        MiniData secret = new MiniData((String) swapLinksMsg.get("secret"));
        if (state.getIpReqSecret().isEqual(secret)) {
            String hostIP = (String) swapLinksMsg.get("res_ip");
            state.setMyMinimaAddress(hostIP);
            state.setHostSet(true);
            state.getKnownPeers().remove(state.getMyMinimaAddress());
            P2PFunctions.log_debug("[+] Setting My IP: " + hostIP);

        }

    }

    public static List<Message> joinScaleOutLinks(P2PState state, int targetNumLinks, ArrayList<NIOClientInfo> clients) {
        List<Message> sendMsgs = new ArrayList<>();
        if (state.getOutLinks().size() < targetNumLinks) {
            P2PFunctions.log_debug("Attempting to scale outlinks");
            InetSocketAddress nextHop = UtilFuncs.selectRandomAddress(new ArrayList<>(state.getOutLinks().values()));
            NIOClientInfo minimaClient = UtilFuncs.getClientFromInetAddress(nextHop, state);
            if (minimaClient != null) {
                P2PWalkLinks walkLinksMsg = new P2PWalkLinks(true, true, minimaClient.getUID());
                sendMsgs.add(new Message(P2PManager.P2P_SEND_MSG).addString("uid", minimaClient.getUID()).addObject("json", walkLinksMsg.toJson()));
            }
        }
        return sendMsgs;
    }

    public static List<Message> requestInLinks(P2PState state, int targetNumLinks, ArrayList<NIOClientInfo> clients) {
        List<Message> sendMsgs = new ArrayList<>();
        if (state.isAcceptingInLinks() && state.getInLinks().size() < targetNumLinks) {
            P2PFunctions.log_debug("Attempting to scale inlinks");
            InetSocketAddress nextHop = UtilFuncs.selectRandomAddress(new ArrayList<>(state.getOutLinks().values()));
            NIOClientInfo minimaClient = UtilFuncs.getClientFromInetAddress(nextHop, state);
            if (minimaClient != null) {
                P2PWalkLinks walkLinksMsg = new P2PWalkLinks(false, false, minimaClient.getUID());
                sendMsgs.add(new Message(P2PManager.P2P_SEND_MSG).addString("uid", minimaClient.getUID()).addObject("json", walkLinksMsg.toJson()));
            }
        }
        return sendMsgs;
    }

}
