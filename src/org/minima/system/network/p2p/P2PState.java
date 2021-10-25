package org.minima.system.network.p2p;

import com.google.common.collect.EvictingQueue;
import lombok.Getter;
import lombok.Setter;
import org.minima.GlobalParams;
import org.minima.objects.base.MiniData;
import org.minima.system.network.base.MinimaClient;
import org.minima.system.network.p2p.messages.ExpiringMessage;
import org.minima.system.network.p2p.messages.P2PMsgNode;
import org.minima.utils.MinimaLogger;
import org.minima.utils.messages.Message;

import java.io.File;
import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

@Getter
@Setter
public class P2PState {

    /**
     * Node's role is a client in the network.
     * Either stated at startup as parameter or derived by no inbound connections being created after startup and rendezvous.
     */
    private boolean isClient = false;
    private boolean isRendezvousComplete = false;
    private boolean isEntryNodeConnected = false;
    private boolean isSetupComplete = false;
    private InetSocketAddress address;
    private final File p2pDataFile;

    private ArrayList<InetSocketAddress> inLinks = new ArrayList<>();
    private ArrayList<InetSocketAddress> outLinks = new ArrayList<>();
    private ArrayList<InetSocketAddress> clientLinks = new ArrayList<>();

    private EvictingQueue<InetSocketAddress> recentJoiners = EvictingQueue.create(10);

    private Map<MiniData, ExpiringMessage> expiringMessageMap = new HashMap<>();

    private ArrayList<String> disconnectingClients = new ArrayList<>();

    private Map<InetSocketAddress, ConnectionDetails> connectionDetailsMap = new HashMap<>();
    private Map<String, Long> expectedAuthKeys = new HashMap<>();


    private Map<InetSocketAddress, P2PMsgNode> networkMap = new HashMap<>();
    private Map<InetSocketAddress, MinimaClient> activeMappingRequests = new HashMap<>();

    /**
     * Maps the hopped message secret to a routing message
     * When we receive a message that is in this map,
     * we know where to send it next
     */
    private Map<MiniData, Message> messageReturnHopMap;

    public String genPrintableState() {
        StringBuilder inLinksStr = new StringBuilder();
        for (InetSocketAddress linkAddr : inLinks) {
            inLinksStr.append("\nP2P\t\t").append(linkAddr);
        }
        StringBuilder outLinksStr = new StringBuilder();
        for (InetSocketAddress linkAddr : outLinks) {
            outLinksStr.append("\nP2P\t\t").append(linkAddr);
        }
        StringBuilder clientLinksStr = new StringBuilder();
        for (InetSocketAddress linkAddr : clientLinks) {
            clientLinksStr.append("\nP2P\t\t").append(linkAddr);
        }
        StringBuilder randomStr = new StringBuilder();
        for (InetSocketAddress linkAddr : recentJoiners) {
            randomStr.append("\nP2P\t\t").append(linkAddr);
        }
        StringBuilder detailsStr = new StringBuilder();
        for (InetSocketAddress linkAddr : connectionDetailsMap.keySet()) {
            detailsStr.append("\nP2P\t\t").append(linkAddr).append(": ").append(connectionDetailsMap.get(linkAddr).getReason().toString());
        }

        return "\n[+] P2P State" +
                "\nP2P\tisClient: " + isClient +
                "\nP2P\tisRendezvousComplete: " + isRendezvousComplete +
                "\nP2P\taddress: " + address +
                "\nP2P\tinLinks: " + inLinksStr +
                "\nP2P\toutLinks: " + outLinksStr +
                "\nP2P\tclientLinks: " + clientLinksStr +
                "\nP2P\trandomNodeSet: " + randomStr +
                "\nP2P\texpiringMessageMap: " + expiringMessageMap.size() +
                "\nP2P\tconnectionDetails: " + detailsStr;
    }

    public P2PState(File p2pDataFile) {
        this.p2pDataFile = p2pDataFile;
    }

    public void addInLink(InetSocketAddress address) {
        this.inLinks.add(address);
        MinimaLogger.log(this.genPrintableState());
    }

    public boolean removeInLink(String uid) {
        return this.inLinks.remove(address);
    }

    public void addOutLink(InetSocketAddress address) {
        this.outLinks.add(address);
        MinimaLogger.log(this.genPrintableState());
    }

    public boolean removeOutLink(InetSocketAddress address) {
        return this.outLinks.remove(address);
    }

    public void addClientLink(InetSocketAddress address) {
        this.clientLinks.add(address);
        MinimaLogger.log(this.genPrintableState());
    }

    public boolean removeClientLink(String uid) {
        return this.clientLinks.remove(address);
    }

    public boolean removeDisconnectingClient(String uid) {
        return this.disconnectingClients.remove(uid);
    }

    public void removeLink(MinimaClient client) {
        if (!client.isIncoming()){
            this.outLinks.remove(client.getMinimaAddress());
        } else {
            if (client.isClient()){
                this.clientLinks.remove(client.getMinimaAddress());
            } else {
                this.inLinks.remove(client.getMinimaAddress());
            }
        }
        MinimaLogger.log(this.genPrintableState());
    }

    public void addRandomNodeSet(InetSocketAddress address) {
        this.recentJoiners.add(address);
    }

    public boolean removeRandomNodeSet(InetSocketAddress address) {
        return this.recentJoiners.remove(address);
    }


    public int countActiveMessagesOfType(String msgType) {
        return (int) expiringMessageMap.values().stream().filter(x -> x.getMsg().isMessageType(msgType)).count();
    }

    public ArrayList<ExpiringMessage> dropExpiredMessages() {

        ArrayList<ExpiringMessage> expiringMessages = new ArrayList<>();
        ArrayList<MiniData> toRemove = new ArrayList<>();
        if (!expiringMessageMap.isEmpty()) {
            for (MiniData key : expiringMessageMap.keySet()) {
                ExpiringMessage msg = expiringMessageMap.get(key);
                if (msg.getTimestamp() < System.currentTimeMillis()) {
                    toRemove.add(key);
                    expiringMessages.add(msg);
                }
            }
        }
        toRemove.forEach(expiringMessageMap::remove);
        return expiringMessages;
    }
}
