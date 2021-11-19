package org.minima.system.network.p2p;

import java.net.InetSocketAddress;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.minima.database.MinimaDB;
import org.minima.objects.base.MiniData;
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


    /**
     * Set of known peers
     */
    private Set<InetSocketAddress> knownPeers = new HashSet<>();

    /**
     * The hosts Minima Address
     */
    private InetSocketAddress myMinimaAddress = null;

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
        json.put("address", myMinimaAddress.toString().replace("/", ""));
        json.put("timestamp", Instant.ofEpochMilli(System.currentTimeMillis()).toString());
        json.put("minima_version", GlobalParams.MINIMA_VERSION);
        json.put("top_block_number", MinimaDB.getDB().getTxPoWTree().getTip().getBlockNumber());
        json.put("is_mobile", GeneralParams.IS_MOBILE);
        json.put("out_links", addressListToJSONArray(new ArrayList<>(outLinks.values())));
        json.put("in_links", addressListToJSONArray(new ArrayList<>(inLinks.values())));
        json.put("not_accepting_conn_links", addressListToJSONArray(new ArrayList<>(notAcceptingConnP2PLinks.values())));
        json.put("none_p2p_links", addressListToJSONArray(new ArrayList<>(noneP2PLinks.values())));
        json.put("knownPeers", addressListToJSONArray(new ArrayList<>(knownPeers)));
        json.put("is_accepting_connections", isAcceptingInLinks);
        return json;
    }

    private JSONArray addressListToJSONArray(ArrayList<InetSocketAddress> addresses) {
        JSONArray links = new JSONArray();
        if (!addresses.isEmpty()) {
            for (InetSocketAddress inetSocketAddress : addresses) {
                if (inetSocketAddress != null) {
                    links.add(inetSocketAddress.toString().replaceAll("/", ""));
                }
            }
        }
        return links;
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
}
