package org.minima.system.network.p2p.functions;

import lombok.extern.slf4j.Slf4j;
import org.minima.objects.base.MiniData;
import org.minima.objects.greet.Greeting;
import org.minima.system.network.base.MinimaClient;
import org.minima.system.network.p2p.ConnectionReason;
import org.minima.system.network.p2p.P2PMessageProcessor;
import org.minima.system.network.p2p.P2PState;
import org.minima.system.network.p2p.messages.*;
import org.minima.utils.messages.Message;

import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.stream.Collectors;


@Slf4j
public class GreetingFuncs {

    public static ArrayList<Message> onGreetedMsg(P2PState state, P2PMsgGreeting greeting, MinimaClient client, ArrayList<MinimaClient> allClients) {
        ArrayList<Message> retMsgs = new ArrayList<>();
        // If this message is coming from a p2p aware minima version

        InetSocketAddress address = new InetSocketAddress(client.getHost(), greeting.getMinimaPort());
        client.setMinimaAddress(address);
        client.setIsClient(greeting.isClient());

        StringBuilder reasonString = new StringBuilder();
        reasonString.append("[+] P2P_GREETING reason: ").append(greeting.getReason());
        if (!client.isIncoming()){
            reasonString.append(" for outbound connection");
        }
        log.debug(reasonString.toString());


        // InLinks
        switch (greeting.getReason()) {
            case NONE:
                if (client.isIncoming()) {
                    state.getInLinks().add(client.getMinimaAddress());
                    log.warn("[-] P2P Incoming connection with no reason");
                }
                break;
            case RENDEZVOUS:
                retMsgs.addAll(onRendezvousGreeting(state, client));
                break;
            case ENTRY_NODE:
                retMsgs.addAll(onEntryNodeGreeting(state, client));
                if (state.isRendezvousComplete()) {
                    retMsgs.addAll(genClientLoadBalanceRequests(state, client.getMinimaAddress(), greeting.getNumClientSlotsAvailable()));
                }
                break;
            case DO_SWAP:
                retMsgs.addAll(onDoSwapGreeting(state, client, greeting.getAuth_key()));
                break;
            case ADDING_OUT_LINK:
                retMsgs.addAll(onAddOutLinkGreeting(state, client, greeting.getAuth_key()));
                if (state.isRendezvousComplete()) {
                    retMsgs.addAll(genClientLoadBalanceRequests(state, client.getMinimaAddress(), greeting.getNumClientSlotsAvailable()));
                }
                break;
            case REPLACING_OUT_LINK:
                retMsgs.addAll(onReplacingOutLinkGreeting(state, client, greeting.getAuth_key()));
                if (state.isRendezvousComplete()) {
                    retMsgs.addAll(genClientLoadBalanceRequests(state, client.getMinimaAddress(), greeting.getNumClientSlotsAvailable()));
                }
                break;
            case CLIENT:
                retMsgs.addAll(onClientGreeting(state, client, allClients));
                break;
            case MAPPING:
                retMsgs.addAll(onMappingGreeting(state, client));
            default:
                break;
        }


        if (!greeting.isClient() && state.getRecentJoiners().size() < 5 && !state.getRecentJoiners().contains(address) && !address.equals(state.getAddress())) {
            state.getRecentJoiners().add(address);
        }

        log.debug(state.genPrintableState());
        return retMsgs;
    }

    public static ArrayList<Message> onRendezvousGreeting(P2PState state, MinimaClient client) {
        ArrayList<Message> retMsgs = new ArrayList<>();

        P2PMsgRendezvous rendezvous = new P2PMsgRendezvous(StartupFuncs.GenRendezvousNodeList(state, 10), client.getMinimaAddress());
//        if (rendezvous.getAddresses().isEmpty()){
//            rendezvous.getAddresses().add(state.getAddress());
//        }
        Message message = new Message(MinimaClient.NETCLIENT_P2P_RENDEZVOUS)
                .addObject("data", rendezvous);

        retMsgs.add(new Message(P2PMessageProcessor.P2P_SEND_MESSAGE)
                .addObject("client", client)
                .addObject("message", message)
        );

//        retMsgs.add(new Message(P2PMessageProcessor.P2P_DISCONNECT)
//                .addObject("client", client)
//                .addInteger("attempt", 0)
//                .addString("reason", "Disconnecting after sending rendezvous message")
//        );

        return retMsgs;
    }

    public static ArrayList<Message> onEntryNodeGreeting(P2PState state, MinimaClient client) {
        ArrayList<Message> retMsgs = new ArrayList<>();

//        if (state.getInLinks().contains(client.getMinimaAddress()) || state.getClientLinks().contains(client.getMinimaAddress())) {
//            retMsgs.add(new Message(P2PMessageProcessor.P2P_DISCONNECT)
//                    .addObject("client", client)
//                    .addInteger("attempt", 0)
//                    .addString("reason", "Disconnecting as reason for connection is ENTRY_NODE but it already has an incoming connection to this node")
//            );
//        } else {
        client.setIsTemp(false);
        state.addInLink(client.getMinimaAddress());
        log.debug(state.genPrintableState());
//        }

        return retMsgs;
    }

    public static ArrayList<Message> onDoSwapGreeting(P2PState state, MinimaClient client, MiniData authKey) {
        ArrayList<Message> retMsgs = new ArrayList<>();

        if (state.getExpectedAuthKeys().containsKey(authKey.toString())) {
            state.getExpectedAuthKeys().remove(authKey.toString());
            state.addInLink(client.getMinimaAddress());
            client.setIsTemp(false);
            log.debug(state.genPrintableState());

        } else {
            retMsgs.add(new Message(P2PMessageProcessor.P2P_DISCONNECT)
                    .addObject("client", client)
                    .addInteger("attempt", 0)
                    .addString("reason", "Disconnecting DO_SWAP link as didn't provide a valid auth key: " + authKey + " valid keys: " + state.getExpectedAuthKeys().keySet())
            );
        }

        return retMsgs;
    }

    public static ArrayList<Message> onAddOutLinkGreeting(P2PState state, MinimaClient client, MiniData authKey) {
        ArrayList<Message> retMsgs = new ArrayList<>();

        if (state.getExpectedAuthKeys().containsKey(authKey.toString())) {
            client.setIsTemp(false);
            state.getExpectedAuthKeys().remove(authKey.toString());
            state.addInLink(client.getMinimaAddress());
            log.debug(state.genPrintableState());

            if (state.getInLinks().size() >= state.getNumLinks()) {
                P2PMsgSwapLink swapLink = new P2PMsgSwapLink();
                swapLink.setSwapTarget(client.getMinimaAddress());
                swapLink.setSecret(authKey);
                retMsgs.add(
                        new Message(P2PMessageProcessor.P2P_SWAP_LINK).addObject("data", swapLink)
                );
            }

        } else {
            retMsgs.add(new Message(P2PMessageProcessor.P2P_DISCONNECT)
                    .addObject("client", client)
                    .addInteger("attempt", 0)
                    .addString("reason", "Disconnecting add outLink as didn't provide a valid auth key: " + authKey + " valid keys: " + state.getExpectedAuthKeys().keySet())
            );
        }

        return retMsgs;
    }

    public static ArrayList<Message> onReplacingOutLinkGreeting(P2PState state, MinimaClient client, MiniData authKey) {
        ArrayList<Message> retMsgs = new ArrayList<>();

        if (state.getExpectedAuthKeys().containsKey(authKey.toString())) {
            client.setIsTemp(false);
            state.getExpectedAuthKeys().remove(authKey.toString());
            state.addInLink(client.getMinimaAddress());
            log.debug(state.genPrintableState());
        } else {
            retMsgs.add(new Message(P2PMessageProcessor.P2P_DISCONNECT)
                    .addObject("client", client)
                    .addInteger("attempt", 0)
                    .addString("reason", "Disconnecting replace outLink as didn't provide a valid auth key: " + authKey + " valid keys: " + state.getExpectedAuthKeys().keySet())
            );
        }

        return retMsgs;
    }

    public static ArrayList<Message> onClientGreeting(P2PState state, MinimaClient client, ArrayList<MinimaClient> minimaClients) {
        ArrayList<Message> retMsgs = new ArrayList<>();
        client.setIsClient(true);
        if (state.getClientLinks().contains(client.getMinimaAddress())) {
            retMsgs.add(new Message(P2PMessageProcessor.P2P_DISCONNECT)
                    .addObject("client", client)
                    .addInteger("attempt", 0)
                    .addString("reason", "Disconnecting as already connected to this client")
            );
        } else {
            client.setIsTemp(false);
            state.addClientLink(client.getMinimaAddress());
            log.debug(state.genPrintableState());
            if (state.isSetupComplete() && !state.isClient() && state.getOutLinks().size() < state.getNumLinks()) {
                // Replace Outlink
                P2PMsgWalkLinks walkLinks = new P2PMsgWalkLinks(true, false);
                walkLinks.setClientWalk(true);
                InetSocketAddress nextHop = UtilFuncs.SelectRandomAddress(state.getOutLinks());
                MinimaClient minimaClient = UtilFuncs.getClientForInetAddress(nextHop, minimaClients, false);
                retMsgs.add(WalkLinksFuncs.genP2PWalkLinkMsg(state, minimaClient, walkLinks, "P2P_ON_DISCONNECTED"));
            }

        }

        return retMsgs;
    }

    public static ArrayList<Message> onMappingGreeting(P2PState state, MinimaClient client) {
        ArrayList<Message> retMsgs = new ArrayList<>();

        P2PMsgNode mapping = new P2PMsgNode(state);

        Message message = new Message(MinimaClient.NETMESSAGE_P2P_MAP_NETWORK)
                .addObject("data", mapping);

        retMsgs.add(new Message(P2PMessageProcessor.P2P_SEND_MESSAGE)
                .addObject("client", client)
                .addObject("message", message)
        );

//        retMsgs.add(new Message(P2PMessageProcessor.P2P_DISCONNECT)
//                .addObject("client", client)
//                .addInteger("attempt", 0)
//                .addString("reason", "Disconnecting after sending mapping message")
//        );

        return retMsgs;
    }

    public static ArrayList<Message> genClientLoadBalanceRequests(P2PState state, InetSocketAddress address, int maxClientsCanReceive) {

        ArrayList<Message> retMsgs = new ArrayList<>();
        // TODO: Add * 2 to reduce the amount of load balancing messages
        if (state.getClientLinks().size() > state.getNumLinks()) {
            int numClientsToSend = state.getClientLinks().size() / 2;
            int numSwaps = Math.min(numClientsToSend, maxClientsCanReceive);
            for (int i = 0; i < numSwaps; i++) {
                // Send a DOSWAP message to numSwaps clients
                P2PMsgSwapLink swapLink = new P2PMsgSwapLink();
                swapLink.setSwapTarget(address);
                swapLink.setSwapClientReq(true);
                retMsgs.add(new Message(P2PMessageProcessor.P2P_SWAP_LINK).addObject("data", swapLink));
            }
        }
        return retMsgs;
    }
}
