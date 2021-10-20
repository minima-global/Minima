package org.minima.system.network.p2p;

import com.google.common.collect.*;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.minima.objects.base.MiniData;
import org.minima.system.network.base.MinimaClient;
import org.minima.system.network.p2p.event.EventPublisher;
import org.minima.system.network.p2p.messages.ExpiringMessage;
import org.minima.system.network.p2p.messages.P2PMsgNode;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

import java.io.File;
import java.net.InetSocketAddress;
import java.util.*;

import static java.util.Collections.copy;
import static org.minima.system.network.p2p.util.JSONObjectUtils.from;
import static org.minima.system.network.p2p.util.MapUtils.MapBuilder.*;

@Slf4j
public class P2PState {

    public static final int RENDEZVOUS_SHUTDOWN_DELAY = 1_000;
    public static final int AUTH_KEY_EXPIRY = 300_000;

    @Getter
    private int numLinks;
    /**
     * Node's role is a client in the network.
     * Either stated at startup as parameter or derived by no inbound connections being created after startup and rendezvous.
     */
    @Getter
    private boolean isClient = false;
    @Getter
    private boolean isRendezvousComplete = false;
    @Getter
    private boolean isEntryNodeConnected = false;
    @Getter
    private boolean isSetupComplete = false;
    @Getter
    private InetSocketAddress address;
    @Getter
    private final File p2pDataFile;

    private final ArrayList<InetSocketAddress> inLinks = new ArrayList<>();
    private final ArrayList<InetSocketAddress> outLinks = new ArrayList<>();
    private final ArrayList<InetSocketAddress> clientLinks = new ArrayList<>();

    private final EvictingQueue<InetSocketAddress> recentJoiners = EvictingQueue.create(10);

    private final Map<MiniData, ExpiringMessage> expiringMessageMap = new HashMap<>();

    private final ArrayList<String> disconnectingClients = new ArrayList<>();

    @Getter
    private final Map<InetSocketAddress, ConnectionDetails> connectionDetailsMap = new HashMap<>();
    private final Map<String, Long> expectedAuthKeys = new HashMap<>();

    @Getter
    private final Map<InetSocketAddress, P2PMsgNode> networkMap = new HashMap<>();
    @Getter
    private final Map<InetSocketAddress, MinimaClient> activeMappingRequests = new HashMap<>();

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
                "\nP2P\tnumLinks: " + numLinks +
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

    public P2PState(int numLinks, File p2pDataFile) {
        this.numLinks = numLinks;
        this.p2pDataFile = p2pDataFile;
    }

    public void addInLink(InetSocketAddress address) {
        this.inLinks.add(address);

        EventPublisher.publish("state change - in-link addition attempt",
                new JSONObject(using("address", from(address))
                        .and("success", true)
                        .asMap()));

        log.debug(this.genPrintableState());
    }

    public boolean removeInLink(InetSocketAddress address) {
        boolean result = this.inLinks.remove(address);

        EventPublisher.publish("state change - in-links removal attempt",
                new JSONObject(using("address", from(address))
                        .and("success", result)
                        .asMap()));

        return result;
    }

    public void addClientLink(InetSocketAddress address) {
        this.clientLinks.add(address);

        EventPublisher.publish("state change - client-link addition attempt",
                new JSONObject(using("address", from(address))
                        .and("success", true)
                        .asMap()));

        log.debug(this.genPrintableState());
    }

    public boolean removeClientLink(InetSocketAddress address) {
        boolean result = this.clientLinks.remove(address);

        EventPublisher.publish("state change - client-links removal attempt",
                new JSONObject(using("address", from(address))
                        .and("success", result)
                        .asMap()));

        return result;
    }

    public void addOutLink(InetSocketAddress address) {
        this.outLinks.add(address);

        EventPublisher.publish("state change - out-link addition attempt",
                new JSONObject(using("address", from(address))
                        .and("success", true)
                        .asMap()));

        log.debug(this.genPrintableState());
    }

    public boolean removeDisconnectingClient(String uid) {

        boolean result = this.disconnectingClients.remove(uid);

        EventPublisher.publish("state change - disconnecting client removal attempt",
                new JSONObject(using("uid", uid)
                        .and("success", result)
                        .asMap()));

        return result;
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
        EventPublisher.publish("state change - link removal attempt",
                new JSONObject(using("address", from(client.getMinimaAddress()))
                        .asMap()));

        log.debug(this.genPrintableState());
    }

    public void addRandomNodeSet(InetSocketAddress address) {
        this.recentJoiners.add(address);

        EventPublisher.publish("state change - random node set address addition attempt",
                new JSONObject(using("address", from(address))
                        .and("success", true)
                        .asMap()));
    }

    public boolean removeRandomNodeSet(InetSocketAddress address) {
        boolean result = this.recentJoiners.remove(address);

        EventPublisher.publish("state change - random node set address removal attempt",
                new JSONObject(using("address", from(address))
                       .and("success", result)
                        .asMap()));

        return result;
    }

    public int countActiveMessagesOfType(String msgType) {
        return (int) expiringMessageMap.values().stream()
                .filter(x -> x.getMsg().isMessageType(msgType))
                .count();
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

        EventPublisher.publish("state change - dropping expired messages");

        return expiringMessages;
    }

    public void setClient(boolean client) {

        this.isClient = client;
        EventPublisher.publish("state change - client",
                new JSONObject(using("client", client)
                        .asMap()));
    }

    public void rendezvousComplete() {

        isRendezvousComplete = true;
        EventPublisher.publish("state change - rendezvous complete");
    }

    public void entryNodeConnected() {

        isEntryNodeConnected = true;
        EventPublisher.publish("state change - entry node connected");
    }

    public void setAddress(InetSocketAddress address) {
        EventPublisher.publish("state change - address changed",
                new JSONObject(using("from", from(this.address))
                        .and("to", from(address))
                        .asMap()));

        this.address = address;
    }

    public ArrayList<InetSocketAddress> getInLinksCopy() {
        return new ArrayList<>(inLinks);
    }

    public ArrayList<InetSocketAddress> getOutLinksCopy() {
        return new ArrayList<>(outLinks);
    }

    public ArrayList<InetSocketAddress> getClientLinksCopy() {
        return new ArrayList<>(clientLinks);
    }

    public boolean addRecentJoiners(Collection<InetSocketAddress> addresses) {
        EventPublisher.publish("state change - recent joiner addition(s) attempted",
                new JSONObject(using("addresses", from(addresses))
                        .and("success", true)
                        .asMap()));

        return recentJoiners.addAll(addresses);
    }

    public boolean addRecentJoiner(InetSocketAddress address) {
        EventPublisher.publish("state change - recent joiner addition attempted",
                new JSONObject(using("address", from(address))
                        .and("success", true)
                        .asMap()));

        return recentJoiners.add(address);
    }

    public ImmutableCollection<InetSocketAddress> getRecentJoinersCopy() {
        return  ImmutableList.copyOf(recentJoiners);
    }

    public Map<MiniData, ExpiringMessage> getExpiringMessageMapCopy() {
        return ImmutableMap.copyOf(expiringMessageMap);
    }

    public void addExpiringMessage(MiniData miniData, ExpiringMessage msg) {
        EventPublisher.publish("state change - adding auth key addition attempted",
                new JSONObject(using("miniData", miniData)
                        .and("msg", msg.getMsg().getMessageType())
                        .and("inner-msg-type", msg.getMsg().getMessageType())
                        .asMap()));
        expiringMessageMap.put(miniData, msg);
    }

    public ImmutableMap<String, Long> getExpectedAuthKeysCopy() {
        return ImmutableMap.copyOf(expectedAuthKeys);
    }

    public void addExpectedAuthKeyAndValue(String authKey, Long value) {
        EventPublisher.publish("state change - expected auth key addition attempted",
                new JSONObject(using("authKey", authKey)
                        .and("value", value)
                        .asMap()));
        expectedAuthKeys.put(authKey, value);
    }

    public Long removeExpectedAuthKey(String authKey) {

        Long result = expectedAuthKeys.remove(authKey);

        EventPublisher.publish("state change - expected auth key removal attempted",
                new JSONObject(using("authKey", authKey)
                        .and("success", result != null)
                        .asMap()));
        return result;
    }

    public void setNumLinks(int numLinks) {

        EventPublisher.publish("state change - number of links changed",
                new JSONObject(using("from", this.numLinks)
                        .and("to", numLinks)
                        .asMap()));
        this.numLinks = numLinks;
    }

    public List<String> getDisconnectingClientsCopy() {
        return new ArrayList<>(disconnectingClients);

    }
}
