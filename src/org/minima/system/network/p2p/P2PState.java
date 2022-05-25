package org.minima.system.network.p2p;

import java.math.BigInteger;
import java.net.InetSocketAddress;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.minima.database.MinimaDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.network.p2p.messages.InetSocketAddressIO;
import org.minima.system.network.p2p.params.P2PParams;
import org.minima.system.params.GeneralParams;
import org.minima.system.params.GlobalParams;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class P2PState {

    /**
     * Connection maps
     * UID - P2PConnection
     */
    private Map<String, InetSocketAddress> inLinks = new HashMap<>();
    private Map<String, InetSocketAddress> outLinks = new HashMap<>();
    private Map<String, InetSocketAddress> notAcceptingConnP2PLinks = new HashMap<>();
    private Map<String, InetSocketAddress> noneP2PLinks = new HashMap<>();

    private Map<String, InetSocketAddress> allLinks = new HashMap<>();

    /**
     * Set of known peers
     */
    private Set<InetSocketAddress> knownPeers = new HashSet<>();

    /**
     * The hosts Minima Address - set a default
     */
    private InetSocketAddress myMinimaAddress = new InetSocketAddress(9001);

    /**
     * A secret to send with the ip request message
     */
    private MiniData ipReqSecret = new MiniData();

    /**
     * Can this node accept InLinks
     * False if behind a firewall
     */
    private boolean isAcceptingInLinks = true;

    /**
     * The max number of connections this node
     * can take that are not part of the p2p system
     */
    private int maxNumNoneP2PConnections = P2PParams.TGT_NUM_NONE_P2P_LINKS;

    /**
     * The max number of connections this node
     */
    private int maxNumP2PConnections = P2PParams.TGT_NUM_LINKS;

    /**
     * Doing a discovery connection to one of the default
     * nodes
     */
    private boolean doingDiscoveryConnection = false;

    /**
     * The loop delay for the p2p manager
     */
    private long loopDelay = P2PParams.LOOP_DELAY;

    private boolean noConnect = false;

    private boolean isHostSet = false;

    private float deviceHashRate = 0.0f;

    public float getDeviceHashRate() {
        return deviceHashRate;
    }

    public void setDeviceHashRate(float deviceHashRate) {
        this.deviceHashRate = deviceHashRate;
    }

    public P2PState() {
        // Creates a new empty state
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

    public Map<String, InetSocketAddress> getInLinks() {
        return inLinks;
    }

    public void setInLinks(Map<String, InetSocketAddress> inLinks) {
        this.inLinks = inLinks;
    }

    public Map<String, InetSocketAddress> getOutLinks() {
        return outLinks;
    }

    public void setOutLinks(Map<String, InetSocketAddress> outLinks) {
        this.outLinks = outLinks;
    }

    public Map<String, InetSocketAddress> getNoneP2PLinks() {
        return noneP2PLinks;
    }

    public void setNoneP2PLinks(Map<String, InetSocketAddress> noneP2PLinks) {
        this.noneP2PLinks = noneP2PLinks;
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

    public void setMyMinimaAddress(InetSocketAddress myMinimaAddress) {
        this.myMinimaAddress = myMinimaAddress;
    }

    public MiniData getIpReqSecret() {
        return ipReqSecret;
    }

    public void setIpReqSecret(MiniData ipReqSecret) {
        this.ipReqSecret = ipReqSecret;
    }

    public long getLoopDelay() {
        return loopDelay;
    }

    public void setLoopDelay(long loopDelay) {
        this.loopDelay = loopDelay;
    }

    public void setLoopDelayToParamValue() {
        this.loopDelay = P2PParams.LOOP_DELAY;
    }

    public Map<String, InetSocketAddress> getNotAcceptingConnP2PLinks() {
        return notAcceptingConnP2PLinks;
    }

    public void setNotAcceptingConnP2PLinks(Map<String, InetSocketAddress> notAcceptingConnP2PLinks) {
        this.notAcceptingConnP2PLinks = notAcceptingConnP2PLinks;
    }

    public JSONObject toJson() {
        JSONObject json = new JSONObject();
        if (myMinimaAddress != null) {
            json.put("address", myMinimaAddress.toString().replace("/", ""));
        } else {
            json.put("address", "not set");
        }
        json.put("is_mobile", GeneralParams.IS_MOBILE);
        json.put("is_accepting_connections", isAcceptingInLinks);

        json.put("InLinks", InetSocketAddressIO.addressesListToJSONArray(new ArrayList<>(getInLinks().values())));
        json.put("OutLinks", InetSocketAddressIO.addressesListToJSONArray(new ArrayList<>(getOutLinks().values())));
        json.put("NotAcceptingConnP2PLinks", InetSocketAddressIO.addressesListToJSONArray(new ArrayList<>(getNotAcceptingConnP2PLinks().values())));
        json.put("NoneP2PLinks", InetSocketAddressIO.addressesListToJSONArray(new ArrayList<>(getNoneP2PLinks().values())));
        json.put("numAllLinks", getAllLinks().size());
        json.put("numKnownPeers", getKnownPeers().size());

        return json;
    }

    public boolean isNoConnect() {
        return noConnect;
    }

    public void setNoConnect(boolean noConnect) {
        this.noConnect = noConnect;
    }

    public boolean isHostSet() {
        return isHostSet;
    }

    public void setHostSet(boolean hostSet) {
        isHostSet = hostSet;
    }

    public int getMaxNumP2PConnections() {
        return maxNumP2PConnections;
    }

    public void setMaxNumP2PConnections(int maxNumP2PConnections) {
        this.maxNumP2PConnections = maxNumP2PConnections;
    }

    public Map<String, InetSocketAddress> getAllLinks() {
        return allLinks;
    }

    public void setAllLinks(Map<String, InetSocketAddress> allLinks) {
        this.allLinks = allLinks;
    }
}
