package org.minima.system.network.p2p;

import org.minima.objects.base.MiniData;
import org.minima.system.params.GeneralParams;

import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

public class P2PState {

    /**
     * Connection maps
     * UID - P2PConnection
     */
    private HashMap<String, InetSocketAddress> inLinks = new HashMap<>();
    private HashMap<String, InetSocketAddress> outLinks = new HashMap<>();
    private HashMap<String, InetSocketAddress> NoneP2PLinks = new HashMap<>();


    /**
     * Set of known peers
     */
    private Set<InetSocketAddress> knownPeers = new HashSet<InetSocketAddress>();

    /**
     * The hosts Minima Address
     */
    private InetSocketAddress myMinimaAddress = null;

    /**
     * A secret to send with the ip request message
     */
    private MiniData ipReqSecret;

    /**
     * Can this node accept InLinks
     * False if behind a firewall
     */
    private boolean isAcceptingInLinks = true;

    /**
     * The max number of connections this node
     * can take that are not part of the p2p system
     */
    private int maxNumNoneP2PConnections;


    /**
     * Doing a discovery connection to one of the default
     * nodes
     */
    private boolean doingDiscoveryConnection = false;


    public P2PState() {
    }


    public Set<InetSocketAddress> getKnownPeers() {
        return knownPeers;
    }

    public void setKnownPeers(Set<InetSocketAddress> knownPeers) {
        this.knownPeers = knownPeers;
    }

    public boolean isAcceptingInLinks() {
        return isAcceptingInLinks;
    }

    public void setAcceptingInLinks(boolean acceptingInLinks) {
        isAcceptingInLinks = acceptingInLinks;
    }

    public HashMap<String, InetSocketAddress> getInLinks() {
        return inLinks;
    }

    public void setInLinks(HashMap<String, InetSocketAddress> inLinks) {
        this.inLinks = inLinks;
    }

    public HashMap<String, InetSocketAddress> getOutLinks() {
        return outLinks;
    }

    public void setOutLinks(HashMap<String, InetSocketAddress> outLinks) {
        this.outLinks = outLinks;
    }

    public HashMap<String, InetSocketAddress> getNoneP2PLinks() {
        return NoneP2PLinks;
    }

    public void setNoneP2PLinks(HashMap<String, InetSocketAddress> noneP2PLinks) {
        NoneP2PLinks = noneP2PLinks;
    }

    public int getMaxNumNoneP2PConnections() {
        return maxNumNoneP2PConnections;
    }

    public void setMaxNumNoneP2PConnections(int maxNumNoneP2PConnections) {
        this.maxNumNoneP2PConnections = maxNumNoneP2PConnections;
    }

    public boolean isDoingDiscoveryConnection() {
        return doingDiscoveryConnection;
    }

    public void setDoingDiscoveryConnection(boolean doingDiscoveryConnection) {
        this.doingDiscoveryConnection = doingDiscoveryConnection;
    }

    public InetSocketAddress getMyMinimaAddress() {
        return myMinimaAddress;
    }

    public void setMyMinimaAddress(String host) {
        this.myMinimaAddress = new InetSocketAddress(host, GeneralParams.MINIMA_PORT);
    }

    public MiniData getIpReqSecret() {
        return ipReqSecret;
    }

    public void setIpReqSecret(MiniData ipReqSecret) {
        this.ipReqSecret = ipReqSecret;
    }
}
