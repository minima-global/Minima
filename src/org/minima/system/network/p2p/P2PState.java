package org.minima.system.network.p2p;

import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.checkerframework.checker.units.qual.A;
import org.minima.objects.base.MiniData;
import org.minima.system.network.p2p.messages.P2PMsgMapNetwork;
import org.minima.system.network.p2p.messages.P2PNode;
import org.minima.utils.messages.Message;

import java.io.File;
import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

@Getter
@Setter
@Slf4j
public class P2PState {

    private int numLinks;
    private boolean isClient = false;
    private boolean isRendezvousComplete = false;
    private boolean isSetupComplete = false;
    private InetSocketAddress address;
    private final File p2pDataFile;

    private ArrayList<InetSocketAddress> inLinks = new ArrayList<>();
    private ArrayList<InetSocketAddress> outLinks = new ArrayList<>();
    private ArrayList<InetSocketAddress> clientLinks = new ArrayList<>();

    private ArrayList<InetSocketAddress> randomNodeSet = new ArrayList<>();

    private Map<String, InetSocketAddress> inLinkClientUidToMinimaAddress = new HashMap<>();

    private Map<MiniData, ExpiringMessage> expiringMessageMap = new HashMap<>();

    private ArrayList<InetSocketAddress> requestSwapOnConnect = new ArrayList<>();

    private ArrayList<String> disconnectingClients = new ArrayList<>();

    private Map<MiniData, ArrayList<P2PMsgMapNetwork>> networkMapRequests = new HashMap<>();
    private Map<MiniData, MiniData> requestUIDtoMsgUID = new HashMap<>();

    /**
     * Maps the hopped message secret to a routing message
     * When we receive a message that is in this map,
     * we know where to send it next
     */
    private Map<MiniData, Message> messageReturnHopMap;

    public String genPrintableState() {
        StringBuilder inLinksStr = new StringBuilder();
        for (InetSocketAddress linkAddr : inLinks) {
            inLinksStr.append("\n\t\t").append(linkAddr);
        }
        StringBuilder outLinksStr = new StringBuilder();
        for (InetSocketAddress linkAddr : outLinks) {
            outLinksStr.append("\n\t\t").append(linkAddr);
        }
        StringBuilder clientLinksStr = new StringBuilder();
        for (InetSocketAddress linkAddr : clientLinks) {
            clientLinksStr.append("\n\t\t").append(linkAddr);
        }
        StringBuilder randomStr = new StringBuilder();
        for (InetSocketAddress linkAddr : randomNodeSet) {
            randomStr.append("\n\t\t").append(linkAddr);
        }
        StringBuilder mapStr = new StringBuilder();
        for (String key : inLinkClientUidToMinimaAddress.keySet()) {
            InetSocketAddress linkAddr = inLinkClientUidToMinimaAddress.get(key);
            mapStr.append("\n\t\t").append(key).append(": ").append(linkAddr);
        }

        return "\n[+] P2P State" +
                "\n\tnumLinks: " + numLinks +
                "\n\tisClient: " + isClient +
                "\n\tisRendezvousComplete: " + isRendezvousComplete +
                "\n\taddress: " + address +
                "\n\tinLinks: " + inLinksStr +
                "\n\toutLinks: " + outLinksStr +
                "\n\tclientLinks: " + clientLinksStr +
                "\n\trandomNodeSet: " + randomStr +
                "\n\tinLinkClientUidToMinimaAddress: " + mapStr +
                "\n\texpiringMessageMap: " + expiringMessageMap.size();
    }

    public P2PState(int numLinks, File p2pDataFile) {
        this.numLinks = numLinks;
        this.p2pDataFile = p2pDataFile;
    }

    public void addInLink(InetSocketAddress address) {
        this.inLinks.add(address);
        log.debug(this.genPrintableState());
    }

    public boolean removeInLink(String uid) {
        InetSocketAddress address = this.inLinkClientUidToMinimaAddress.remove(uid);
        return this.inLinks.remove(address);
    }

    public void addOutLink(InetSocketAddress address) {
        this.outLinks.add(address);
        log.debug(this.genPrintableState());
    }

    public boolean removeOutLink(InetSocketAddress address) {
        return this.outLinks.remove(address);
    }

    public void addClientLink(InetSocketAddress address) {
        this.clientLinks.add(address);
        log.debug(this.genPrintableState());
    }

    public boolean removeClientLink(String uid) {
        InetSocketAddress address = this.inLinkClientUidToMinimaAddress.remove(uid);
        return this.clientLinks.remove(address);
    }

    public void addRequestSwapOnConnect(InetSocketAddress address) {
        this.requestSwapOnConnect.add(address);
    }

    public boolean removeRequestSwapOnConnect(InetSocketAddress address) {
        return this.requestSwapOnConnect.remove(address);
    }

    public void addDisconnectingClient(String uid) {
        this.disconnectingClients.add(uid);
    }

    public boolean removeDisconnectingClient(String uid) {
        return this.disconnectingClients.remove(uid);
    }

    public void removeLink(String uid, InetSocketAddress outLinkAddress) {
        this.inLinkClientUidToMinimaAddress.remove(uid);
        this.inLinks.remove(outLinkAddress);
        this.outLinks.remove(outLinkAddress);
        this.clientLinks.remove(outLinkAddress);
        log.debug(this.genPrintableState());
    }

    public void addRandomNodeSet(InetSocketAddress address) {
        this.randomNodeSet.add(address);
    }

    public boolean removeRandomNodeSet(InetSocketAddress address) {
        return this.randomNodeSet.remove(address);
    }

    public void addToInLinkClientUidToMinimaAddress(String uid, InetSocketAddress minimaAddress) {
        this.inLinkClientUidToMinimaAddress.put(uid, minimaAddress);
    }

    public int countActiveMessagesOfType(String msgType) {
        return (int) expiringMessageMap.values().stream().filter(x -> x.msg.isMessageType(msgType)).count();
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
