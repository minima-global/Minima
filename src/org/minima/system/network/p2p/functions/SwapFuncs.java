package org.minima.system.network.p2p.functions;

import lombok.extern.slf4j.Slf4j;
import org.minima.system.network.base.MinimaClient;
import org.minima.system.network.p2p.ConnectionDetails;
import org.minima.system.network.p2p.ConnectionReason;
import org.minima.system.network.p2p.P2PMessageProcessor;
import org.minima.system.network.p2p.P2PState;
import org.minima.system.network.p2p.messages.P2PMsgDoSwap;
import org.minima.system.network.p2p.messages.P2PMsgSwapLink;
import org.minima.utils.messages.Message;

import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.stream.Collectors;

@Slf4j
public class SwapFuncs {

    public static Message onSwapClientsReq(P2PState state, P2PMsgSwapLink swapLink, ArrayList<MinimaClient> allClients){
        InetSocketAddress addressToDoSwap = UtilFuncs.SelectRandomAddress(state.getClientLinksCopy());

        ArrayList<MinimaClient> clients = allClients.stream()
                .filter(MinimaClient::isClient)
                .collect(Collectors.toCollection(ArrayList::new));

        log.debug("[!] P2P_SWAP_LINK Num Clients post filter: " + clients.size() + " num disconnecting clients: " + state.getDisconnectingClientsCopy().size() + " num clients: " + allClients.size());
        MinimaClient minimaClient = UtilFuncs.getClientForInetAddress(addressToDoSwap, clients, true);
        return  SwapFuncs.generateDoSwapMessage(state, swapLink, minimaClient);
    }

    public static Message onSwapReq(P2PState state, P2PMsgSwapLink swapLink, ArrayList<MinimaClient> allClients){
        MinimaClient minimaClient = null;


        // SwapTarget
        ArrayList<InetSocketAddress> filteredInLinks = (ArrayList<InetSocketAddress>) state.getInLinksCopy().stream()
                .filter(x -> !x.equals(swapLink.getSwapTarget()))
                .distinct()
                .collect(Collectors.toList());

        // This isn't correct, need to have
        if (!filteredInLinks.isEmpty()) {
            InetSocketAddress addressToDoSwap = UtilFuncs.SelectRandomAddress(filteredInLinks);

            ArrayList<MinimaClient> clients = allClients.stream()
                    .filter(x -> !state.getDisconnectingClientsCopy()
                            .contains(x.getUID())).collect(Collectors.toCollection(ArrayList::new));
            log.debug("[!] P2P_SWAP_LINK  from: " + state.getAddress() + " Num Clients post filter: " + clients.size() + " num disconnecting clients: " + state.getDisconnectingClientsCopy().size() + " num clients: " + allClients.size());
            minimaClient = UtilFuncs.getClientForInetAddress(addressToDoSwap, clients, true);
        }
        return  SwapFuncs.generateDoSwapMessage(state, swapLink, minimaClient);
    }

    public static Message generateDoSwapMessage(P2PState state, P2PMsgSwapLink swapLink, MinimaClient minimaClient){
        Message retMsg = null;

        if (minimaClient != null) {
            log.debug("[+] P2P_DO_SWAP CLIENT Sending do swap message to client");

            P2PMsgDoSwap msgDoSwap = new P2PMsgDoSwap();
            msgDoSwap.setSecret(swapLink.getSecret());
            msgDoSwap.setSwapTarget(swapLink.getSwapTarget());

            Message message = new Message(MinimaClient.NETMESSAGE_P2P_DO_SWAP)
                    .addObject("data", msgDoSwap);

            retMsg = new Message(P2PMessageProcessor.P2P_SEND_MESSAGE)
                    .addObject("client", minimaClient)
                    .addObject("message", message);

        } else {
            log.debug("[-] P2P_DO_SWAP not sent swap - no clients available");

        }
        return retMsg;
    }

    public static ArrayList<Message> executeDoSwap(P2PState state, P2PMsgDoSwap doSwap, MinimaClient client) {
        ArrayList<Message> messages = new ArrayList<>();

        // Connect to new client
        messages.add(new Message(P2PMessageProcessor.P2P_CONNECT)
                .addObject("address", doSwap.getSwapTarget())
                .addString("reason", " for a DO_SWAP Request")
        );
        ConnectionReason reason = ConnectionReason.DO_SWAP;
        if (state.isClient()){
            reason = ConnectionReason.CLIENT;
        }
        state.getConnectionDetailsMap().put(doSwap.getSwapTarget(), new ConnectionDetails(reason, doSwap.getSecret()));

        // Disconnect from old client
        messages.add(new Message(P2PMessageProcessor.P2P_DISCONNECT)
                .addObject("client", client)
                .addInteger("attempt", 0)
                .addString("reason", "Disconnecting after Do Swap to: " + doSwap.getSwapTarget())
        );
        return messages;
    }

}
