package org.minima.system.network.p2p.functions;


import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.checkerframework.checker.units.qual.A;
import org.minima.objects.greet.Greeting;
import org.minima.system.network.base.MinimaClient;
import org.minima.system.network.p2p.messages.*;
import org.minima.system.network.p2p.P2PMessageProcessor;
import org.minima.system.network.p2p.P2PState;
import org.minima.system.network.rpc.RPCClient;
import org.minima.utils.MinimaLogger;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.TimerMessage;

import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.UnknownHostException;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Slf4j(topic="P2P")
public class StartupFuncs {

    private static final String IPV4_PATTERN =
            "^(([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])(\\.(?!$)|$)){4}$";

    public static boolean isValidIPv4(final String email) {
        Matcher matcher = pattern.matcher(email);
        return matcher.matches();
    }
    private static final Pattern pattern = Pattern.compile(IPV4_PATTERN);

    public static ArrayList<InetSocketAddress> LoadExtraPublicMinimaServers(String[] defaultNodeList){
        // Moved from Start
        String[] returnList = {};
        /**
         * Load the EXTRA public Minima servers for 0.98 - 0.99 has different P2P
         */
        try {
            //Get a list of the EXTRA Minima clients..
            String extraclients = RPCClient.sendGET("http://34.118.59.216/pubextra.txt");

            //Now chop them up..
            ArrayList<String> pubextra = new ArrayList<>();
            StringTokenizer strtok = new StringTokenizer(extraclients,",");
            while(strtok.hasMoreElements()) {
                String ip = strtok.nextToken().trim();
                if(isValidIPv4(ip)) {
                    pubextra.add(ip);
                }else {
                    MinimaLogger.log("Invalid Extra Minima Peer : "+ip);
                }
            }

            //Now add all these nodes..
            String[] newnodes = new String[defaultNodeList.length+pubextra.size()];
            for(int i=0;i<defaultNodeList.length;i++) {
                newnodes[i] = defaultNodeList[i];
            }

            for(int i=0;i<pubextra.size();i++) {
                newnodes[defaultNodeList.length+i] = pubextra.get(i);
            }

            //Re-assign!
            returnList = newnodes;

        } catch (Exception e1) {
            MinimaLogger.log(e1);

            //RESET
            returnList = defaultNodeList;

            MinimaLogger.log("Minima Bootstrap nodes reset : "+Arrays.toString(returnList));
        }
        return ConvertToInetSocketAddresses(returnList);
    }

    public static ArrayList<InetSocketAddress> ConvertToInetSocketAddresses(String[] hosts){
        ArrayList<InetSocketAddress> addresses = new ArrayList<>();
        for(String host: hosts){
            for(int i=0; i <3; i++){ // 9001, 10001, 11001
                try{
                    addresses.add(new InetSocketAddress(InetAddress.getByName(host), 9001 + (1000*i)));
                }catch (UnknownHostException e){
                    MinimaLogger.log("Unknown host: " + host);
                }
            }
        }
        return addresses;
    }

    public static void SaveNodeList(P2PState state){
        final ObjectMapper mapper = new ObjectMapper();
        try {
            if(!state.getP2pDataFile().exists()){
                state.getP2pDataFile().createNewFile();
            }
            FileOutputStream fos = new FileOutputStream(state.getP2pDataFile());
            final ByteArrayOutputStream out = new ByteArrayOutputStream();

            mapper.writeValue(out, GenRendezvousNodeList(state, 100));
            out.writeTo(fos);
            fos.close();
        } catch (IOException ioe) {
            log.error("Failed to write data to file: ", ioe);
        }
    }

    public static ArrayList<InetSocketAddress> LoadNodeList(P2PState state, String[] defaultNodeList, boolean noExtraHosts) {
        // Try and load node list from the saved data
        ArrayList<InetSocketAddress> loadedNodeList = new ArrayList<>();
        if (state.getP2pDataFile().exists()) {
            try {
                FileInputStream inputStream = new FileInputStream(state.getP2pDataFile());
                final ObjectMapper mapper = new ObjectMapper();
                loadedNodeList = mapper.readValue(inputStream, new TypeReference<ArrayList<InetSocketAddress>>() {});
                state.setRandomNodeSet(loadedNodeList);
            } catch (IOException ioe) {
                log.error("Error whilst reading in p2pDataFile: ", ioe);
            }
        } else {
            if(noExtraHosts){
                loadedNodeList = ConvertToInetSocketAddresses(defaultNodeList);
            } else {
                loadedNodeList = LoadExtraPublicMinimaServers(defaultNodeList);
            }
        }
        return loadedNodeList;
    }




    /**
     * PRECONDITION: Arrays A and B are provided
     * @param state
     * @return
     */
    public static ArrayList<InetSocketAddress> GenRendezvousNodeList(P2PState state, int numAddrToReturn)
    {
        assert state.getOutLinks() != null : "OutLinks Array is null";
        assert state.getRandomNodeSet() != null : "InLinks Array is null";
        return Stream.of(state.getRandomNodeSet(), state.getOutLinks())
                .flatMap(Collection::stream).distinct().limit(numAddrToReturn).collect(Collectors.toCollection(ArrayList::new));
    }

    public static TimerMessage processOnRendezvousMsg(P2PState state, P2PMsgRendezvous rendezvous, MinimaClient client) {

        rendezvous.getAddresses().forEach(state::addRandomNodeSet);
        state.setRendezvousComplete(true);

        TimerMessage shutdownMsg = new TimerMessage(P2PState.RENDEZVOUS_SHUTDOWN_DELAY, P2PMessageProcessor.P2P_DISCONNECT);
        shutdownMsg.addObject("client", client);
        shutdownMsg.addInteger("attempt", 0);
        shutdownMsg.addString("reason", "Disconnecting After Rendezvous ");
        return shutdownMsg;
    }

    /*
     * SITUATIONS:
     * 1) Message is from a none p2p node
     * 2) Message is from a client
     *
     */
    public static ArrayList<Message> onGreetedMsg(P2PState state,  Greeting greeting, MinimaClient client) {
        ArrayList<Message> retMsgs = new ArrayList<>();
        // If this message is coming from a p2p aware minima version
        if (greeting.getDetails().containsKey("isClient")) {

            boolean isClient = (boolean) greeting.getDetails().get("isClient");
            Long minimaPort = (Long) greeting.getDetails().get("minimaPort");
            Long numClients = (Long) greeting.getDetails().get("numClients");

            InetSocketAddress address = new InetSocketAddress(client.getHost(), minimaPort.intValue());
            client.setMinimaAddress(address);

            // Update State
            StartupFuncs.updateLinks(state, client, isClient);

            if (!isClient) {
                // Greet P2P Node
                Message returnMessage = StartupFuncs.onGreetP2PNode(state, client);
                if (returnMessage != null){
                    retMsgs.add(returnMessage);
                }

                if (client.isIncoming()){
                    state.setNumInLinkDisconnects(state.getNumInLinkDisconnects() - 1);
                }

                if (state.isRendezvousComplete() && state.getClientLinks().size() > state.getNumLinks() * 2) {
                    // If we have
                    retMsgs.addAll(StartupFuncs.genClientLoadBalanceRequests(state, numClients));
                }
            }

        }
        return retMsgs;
    }

    public static void updateLinks(P2PState state, MinimaClient client, boolean isClient) {

        if (client.isIncoming()) {
            if (isClient) {
                state.addClientLink(client.getMinimaAddress());
            } else {
                state.addInLink(client.getMinimaAddress());
            }
        } else {
            state.addOutLink(client.getMinimaAddress());
        }
        log.debug(state.genPrintableState());
    }

    public static Message onGreetP2PNode(P2PState state, MinimaClient client){
        Message ret = null;
        if (state.getRequestSwapOnConnect().contains(client.getMinimaAddress())) {
            P2PMsgSwapLink swapLink = new P2PMsgSwapLink();
            swapLink.setSwapTarget(state.getAddress());

            ret = new Message(MinimaClient.NETMESSAGE_P2P_SWAP_LINK).addObject("data", swapLink);

            state.removeRequestSwapOnConnect(client.getMinimaAddress());
        }

        log.info("Adding node to RandomNodeSet");
        if (!state.getRandomNodeSet().contains(client.getMinimaAddress())) {
            state.addRandomNodeSet(client.getMinimaAddress());
        }
        return ret;
    }

    public static ArrayList<Message> genClientLoadBalanceRequests(P2PState state, Long numClients){
        ArrayList<Message> retMsgs = new ArrayList<>();
        int numClientSlotsAvailable = (state.getNumLinks() * 4) - state.getClientLinks().size();
        int numSwaps = Math.min(numClients.intValue() / 2, numClientSlotsAvailable);
        for (int i = 0; i < numSwaps; i++) {
            P2PMsgSwapLink swapLink = new P2PMsgSwapLink();
            swapLink.setSwapTarget(state.getAddress());
            swapLink.setSwapClientReq(true);
            retMsgs.add(new Message(MinimaClient.NETMESSAGE_P2P_SWAP_LINK).addObject("data", swapLink));
        }
        return retMsgs;
    }

    public static ArrayList<Message> Join(P2PState state, ArrayList<MinimaClient> clients) {
        boolean needOutLinks = state.getOutLinks().size() < state.getNumLinks();
        ArrayList<Message> msgs = new ArrayList<>();
        // If either is true we need more links
        if (needOutLinks) {
            // connect
            if (!state.getRandomNodeSet().isEmpty()) {
                if (state.getOutLinks().isEmpty()){
                    InetSocketAddress address = UtilFuncs.SelectRandomAddress(state.getRandomNodeSet());
                    log.info("Joining: " + address);
                    msgs.add(new Message(P2PMessageProcessor.P2P_CONNECT).addObject("address", address).addString("reason", "Entry node connection"));
                }
                else if(state.isRendezvousComplete()){
                    int outLinksNeeded = state.getNumLinks() - state.getOutLinks().size();
                    int maxMsgsToSend = 10 - state.countActiveMessagesOfType(P2PMessageProcessor.P2P_WALK_LINKS);
                    int msgsToSend = Math.min(outLinksNeeded, maxMsgsToSend);
                    if (msgsToSend > 0){
                        P2PMsgWalkLinks walkLinks = new P2PMsgWalkLinks(true, true);
                        walkLinks.addHopToPath(state.getAddress());
                        InetSocketAddress nextHop = UtilFuncs.SelectRandomAddress(state.getOutLinks());
                        MinimaClient minimaClient = UtilFuncs.getClientForInetAddress(nextHop, clients, false);
                        if (minimaClient != null) {
                            log.debug("[+] P2P_JOIN Starting new join walk");
                            Message walkMsg = new Message(MinimaClient.NETCLIENT_P2P_WALK_LINKS);
                            walkMsg.addObject("data", walkLinks);
                            minimaClient.PostMessage(walkMsg);
                            state.getExpiringMessageMap().put(walkLinks.getSecret(), new ExpiringMessage(new Message(P2PMessageProcessor.P2P_WALK_LINKS).addObject("data", walkLinks)));
                        } else {
                            log.warn("[-] P2P_JOIN MinimaClient for " + nextHop + " does not exist in clients list for outlink");
                        }
                    }

                }
            }
        }
        return msgs;
    }

    public static Message onInLinkDisconnected(P2PState state, MinimaClient client, ArrayList<MinimaClient> minimaClients) {
        Message returnMessage = null;
        if (state.getInLinks().size() <= state.getNumLinks()) {
            // Replace Inlink
            P2PMsgWalkLinks walkLinks = new P2PMsgWalkLinks(false, false);
            InetSocketAddress nextHop = UtilFuncs.SelectRandomAddress(state.getOutLinks());
            if (nextHop == null) {
                nextHop = UtilFuncs.SelectRandomAddress(state.getInLinks());
            }
            MinimaClient minimaClient = UtilFuncs.getClientForInetAddress(nextHop, minimaClients, false);
            state.setNumInLinkDisconnects(state.getNumInLinkDisconnects() + 1);
            returnMessage = genP2PWalkLinkMsg(state, minimaClient, walkLinks, nextHop);
        }
        if (client != null) {
            state.removeDisconnectingClient(client.getUID());
            state.removeLink(client.getMinimaAddress());
        }
        return returnMessage;
    }

    public static Message onOutLinkDisconnected(P2PState state, MinimaClient client, ArrayList<MinimaClient> minimaClients) {

        Message returnMessage = null;
        if (state.getOutLinks().size() <= state.getNumLinks()) {
            // Replace Outlink
            P2PMsgWalkLinks walkLinks = new P2PMsgWalkLinks(true, true);
            InetSocketAddress nextHop = UtilFuncs.SelectRandomAddress(state.getInLinks());
            MinimaClient minimaClient = UtilFuncs.getClientForInetAddress(nextHop, minimaClients, true);
            returnMessage = genP2PWalkLinkMsg(state, minimaClient, walkLinks, nextHop);
        }

        state.removeDisconnectingClient(client.getUID());
        state.removeLink(client.getMinimaAddress());
        return returnMessage;
    }

    public static Message genP2PWalkLinkMsg(P2PState state, MinimaClient minimaClient, P2PMsgWalkLinks walkLinks, InetSocketAddress nextHop){
        Message retMsg = null;
        if (minimaClient != null) {
            log.debug("[+] P2P_ON_DISCONNECT P2P_WALK Starting inLink walk to replace lost outLink neighbour");
            retMsg = new Message(P2PMessageProcessor.P2P_SEND_MESSAGE)
                    .addObject("client", minimaClient)
                    .addObject("message", new Message(MinimaClient.NETCLIENT_P2P_WALK_LINKS).addObject("data", walkLinks));
            ExpiringMessage expiringMessage = new ExpiringMessage(new Message(P2PMessageProcessor.P2P_WALK_LINKS).addObject("data", walkLinks));
            expiringMessage.setTimestamp(System.currentTimeMillis() + 5_000L);
            state.getExpiringMessageMap().put(walkLinks.getSecret(), expiringMessage);
        } else {
            log.warn("[-] P2P_ON_DISCONNECT MinimaClient for " + nextHop + " does not exist in clients list for inlink");
        }
        return retMsg;
    }
}
