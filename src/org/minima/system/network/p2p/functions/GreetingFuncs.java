package org.minima.system.network.p2p.functions;

import lombok.extern.slf4j.Slf4j;
import org.minima.objects.base.MiniData;
import org.minima.objects.greet.Greeting;
import org.minima.system.network.base.MinimaClient;
import org.minima.system.network.p2p.ConnectionReason;
import org.minima.system.network.p2p.P2PMessageProcessor;
import org.minima.system.network.p2p.P2PState;
import org.minima.system.network.p2p.messages.P2PMsgMapNetwork;
import org.minima.system.network.p2p.messages.P2PMsgRendezvous;
import org.minima.system.network.p2p.messages.P2PMsgSwapLink;
import org.minima.utils.messages.Message;

import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.Collection;
import java.util.stream.Collectors;
import java.util.stream.Stream;


@Slf4j
public class GreetingFuncs {

    public static ArrayList<Message> onGreetedMsg(P2PState state, Greeting greeting, MinimaClient client) {
        ArrayList<Message> retMsgs = new ArrayList<>();
        // If this message is coming from a p2p aware minima version
        if (greeting.getDetails().containsKey("minimaPort")) {
            Long minimaPort = (Long) greeting.getDetails().get("minimaPort");
            Long numClients = (Long) greeting.getDetails().get("numClients");

            InetSocketAddress address = new InetSocketAddress(client.getHost(), minimaPort.intValue());
            client.setMinimaAddress(address);

            ConnectionReason reason = null;
            MiniData authKey = null;
            if (greeting.getDetails().containsKey("reason")) {
                reason = ConnectionReason.valueOf((String) greeting.getDetails().get("reason"));
                log.debug("[+] P2P_GREETING reason: " + reason);
            } else {
                log.warn("[-] P2P_GREETING no reason");
            }

            if (greeting.getDetails().containsKey("auth_key")) {
                authKey = new MiniData((String) greeting.getDetails().get("auth_key"));
            } else {
                authKey = new MiniData();
            }
            if (reason == null) {
                // This message is being received by a node that has just done an outgoing connection
                if (client.isIncoming()) {
                    state.getInLinks().add(client.getMinimaAddress());
                    log.warn("[-] P2P Incoming connection with no reason");
                } else {
                    state.getOutLinks().add(client.getMinimaAddress());
                }
            } else {
                switch (reason) {
                    case RENDEZVOUS:
                        retMsgs.addAll(onRendezvousGreeting(state, client));
                        break;
                    case ENTRY_NODE:
                        retMsgs.addAll(onEntryNodeGreeting(state, client));
                        break;
                    case DO_SWAP:
                        retMsgs.addAll(onDoSwapGreeting(state, client, authKey));
                        break;
                    case ADDING_OUT_LINK:
                        retMsgs.addAll(onAddOutLinkGreeting(state, client, authKey));
                        break;
                    case REPLACING_OUT_LINK:
                        retMsgs.addAll(onReplacingOutLinkGreeting(state, client, authKey));
                        break;
                    case CLIENT:
                        retMsgs.addAll(onClientGreeting(state, client));
                        break;
                    case MAPPING:
                        retMsgs.addAll(onMappingGreeting(state, client));
                    default:
                        break;
                }
            }

            if (reason != ConnectionReason.CLIENT &&
                    state.isRendezvousComplete() &&
                    state.getClientLinks().size() > state.getNumLinks() * 2) {
                retMsgs.addAll(genClientLoadBalanceRequests(state, numClients));
            }

            if (state.getRandomNodeSet().size() < 5 && !state.getRandomNodeSet().contains(address) && !address.equals(state.getAddress())) {
                state.getRandomNodeSet().add(address);
            }

        }
        return retMsgs;
    }

    public static ArrayList<Message> onRendezvousGreeting(P2PState state, MinimaClient client) {
        ArrayList<Message> retMsgs = new ArrayList<>();

        P2PMsgRendezvous rendezvous = new P2PMsgRendezvous(StartupFuncs.GenRendezvousNodeList(state, 10));
//        if (rendezvous.getAddresses().isEmpty()){
//            rendezvous.getAddresses().add(state.getAddress());
//        }
        Message message = new Message(MinimaClient.NETCLIENT_P2P_RENDEZVOUS)
                .addObject("data", rendezvous);

        retMsgs.add(new Message(P2PMessageProcessor.P2P_SEND_MESSAGE)
                .addObject("client", client)
                .addObject("message", message)
        );

        retMsgs.add(new Message(P2PMessageProcessor.P2P_DISCONNECT)
                .addObject("client", client)
                .addInteger("attempt", 0)
                .addString("reason", "Disconnecting after sending rendezvous message")
        );

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
            state.getExpectedAuthKeys().remove(authKey.toString());
            state.addInLink(client.getMinimaAddress());
            log.debug(state.genPrintableState());


            P2PMsgSwapLink swapLink = new P2PMsgSwapLink();
            swapLink.setSwapTarget(client.getMinimaAddress());
            swapLink.setSecret(authKey);
            retMsgs.add(
                    new Message(P2PMessageProcessor.P2P_SWAP_LINK).addObject("data", swapLink)
            );

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

    public static ArrayList<Message> onClientGreeting(P2PState state, MinimaClient client) {
        ArrayList<Message> retMsgs = new ArrayList<>();

        if (state.getClientLinks().contains(client.getMinimaAddress())) {
            retMsgs.add(new Message(P2PMessageProcessor.P2P_DISCONNECT)
                    .addObject("client", client)
                    .addInteger("attempt", 0)
                    .addString("reason", "Disconnecting as already connected to this client")
            );
        } else {
            state.addClientLink(client.getMinimaAddress());
            log.debug(state.genPrintableState());

        }

        return retMsgs;
    }

    public static ArrayList<Message> onMappingGreeting(P2PState state, MinimaClient client) {
        ArrayList<Message> retMsgs = new ArrayList<>();

        P2PMsgMapNetwork mapping = new P2PMsgMapNetwork(
                Stream.of(state.getInLinks(), state.getOutLinks())
                        .flatMap(Collection::stream).distinct().collect(Collectors.toCollection(ArrayList::new)),
                state.getClientLinks().size()
        );

        Message message = new Message(MinimaClient.NETMESSAGE_P2P_MAP_NETWORK)
                .addObject("data", mapping);

        retMsgs.add(new Message(P2PMessageProcessor.P2P_SEND_MESSAGE)
                .addObject("client", client)
                .addObject("message", message)
        );

        retMsgs.add(new Message(P2PMessageProcessor.P2P_DISCONNECT)
                .addObject("client", client)
                .addInteger("attempt", 0)
                .addString("reason", "Disconnecting after sending mapping message")
        );

        return retMsgs;
    }

    public static ArrayList<Message> genClientLoadBalanceRequests(P2PState state, Long numClients) {
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
}
