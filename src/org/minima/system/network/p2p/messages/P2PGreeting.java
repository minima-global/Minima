package org.minima.system.network.p2p.messages;

import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.List;

import org.minima.system.network.p2p.P2PState;
import org.minima.system.params.GeneralParams;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class P2PGreeting {

    /**
     * The Minima Protocol Port
     */
    private int myMinimaPort;

    /**
     * Can this node accept InLinks
     * False if behind a firewall
     */
    private boolean isAcceptingInLinks;

    /**
     * List of connected OutLinks
     */
    private List<InetSocketAddress> outLinks = new ArrayList<>();

    /**
     * List of connected InLinks
     */
    private List<InetSocketAddress> inLinks = new ArrayList<>();

    /**
     * Number of connections that are not part of
     * this p2p system.
     */
    private int numNoneP2PConnections;

    /**
     * The max number of connections this node
     * can take that are not part of the p2p system
     */
    private int maxNumNoneP2PConnections;

    /**
     * List of known P2P Peers
     */
    private List<InetSocketAddress> knownPeers = new ArrayList<>();

    public P2PGreeting() {
    }

    public P2PGreeting(P2PState state) {

        this.myMinimaPort = GeneralParams.MINIMA_PORT;
        this.isAcceptingInLinks = state.isAcceptingInLinks();
        this.outLinks = new ArrayList<>(state.getOutLinks().values());
        this.inLinks = new ArrayList<>(state.getInLinks().values());
        this.numNoneP2PConnections = state.getNoneP2PLinks().size();
        this.maxNumNoneP2PConnections = state.getMaxNumNoneP2PConnections();
        this.knownPeers = new ArrayList<>(state.getKnownPeers());
        if (state.getMyMinimaAddress() != null && state.isAcceptingInLinks()) {
            this.knownPeers.add(state.getMyMinimaAddress());
        }
    }

    /**
     * Constructor for when the node can't accept inLinks
     */
    public P2PGreeting(int myMinimaPort, List<InetSocketAddress> knownPeers) {
        this.myMinimaPort = myMinimaPort;
        this.knownPeers = knownPeers;
        this.isAcceptingInLinks = false;
    }

    /**
     * Constructor for a full P2P Node
     */
    public P2PGreeting(int myMinimaPort, boolean isAcceptingInLinks, List<InetSocketAddress> outLinks, List<InetSocketAddress> inLinks, int numNoneP2PConnections, int maxNumNoneP2PConnections, List<InetSocketAddress> knownPeers) {
        this.myMinimaPort = myMinimaPort;
        this.isAcceptingInLinks = isAcceptingInLinks;
        this.outLinks = outLinks;
        this.inLinks = inLinks;
        this.numNoneP2PConnections = numNoneP2PConnections;
        this.maxNumNoneP2PConnections = maxNumNoneP2PConnections;
        this.knownPeers = knownPeers;
    }



    public static P2PGreeting fromJSON(JSONObject jsonObject) {
        P2PGreeting greeting = new P2PGreeting();
        greeting.setMyMinimaPort(InetSocketAddressIO.safeReadInt(jsonObject, "myMinimaPort"));
        greeting.setAcceptingInLinks((boolean) jsonObject.getOrDefault("isAcceptingInLinks", false));
        greeting.setKnownPeers(InetSocketAddressIO.addressesJSONToList((JSONArray) jsonObject.getOrDefault("knownPeers", new JSONArray())));
        if (greeting.isAcceptingInLinks()) {
            greeting.setOutLinks(InetSocketAddressIO.addressesJSONToList((JSONArray) jsonObject.getOrDefault("outLinks", new JSONArray())));
            greeting.setInLinks(InetSocketAddressIO.addressesJSONToList((JSONArray) jsonObject.getOrDefault("inLinks", new JSONArray())));
            greeting.setNumNoneP2PConnections(InetSocketAddressIO.safeReadInt(jsonObject, "numNoneP2PConnections"));
            greeting.setMaxNumNoneP2PConnections(InetSocketAddressIO.safeReadInt(jsonObject, "maxNumNoneP2PConnections"));
        }
        return greeting;
    }


    public JSONObject toJson() {
        JSONObject json = new JSONObject();
        json.put("myMinimaPort", myMinimaPort);
        json.put("isAcceptingInLinks", isAcceptingInLinks);
        if (isAcceptingInLinks) {
            json.put("numNoneP2PConnections", numNoneP2PConnections);
            json.put("maxNumNoneP2PConnections", maxNumNoneP2PConnections);
            json.put("outLinks", InetSocketAddressIO.addressesListToJSON(outLinks));
            json.put("inLinks", InetSocketAddressIO.addressesListToJSON(inLinks));
        }
        json.put("knownPeers", InetSocketAddressIO.addressesListToJSON(knownPeers));
        JSONObject message = new JSONObject();
        message.put("greeting", json);
        return message;
    }

    public int getMyMinimaPort() {
        return myMinimaPort;
    }

    public void setMyMinimaPort(int myMinimaPort) {
        this.myMinimaPort = myMinimaPort;
    }

    public boolean isAcceptingInLinks() {
        return isAcceptingInLinks;
    }

    public void setAcceptingInLinks(boolean acceptingInLinks) {
        isAcceptingInLinks = acceptingInLinks;
    }

    public List<InetSocketAddress> getOutLinks() {
        return outLinks;
    }

    public void setOutLinks(List<InetSocketAddress> outLinks) {
        this.outLinks = outLinks;
    }

    public List<InetSocketAddress> getInLinks() {
        return inLinks;
    }

    public void setInLinks(List<InetSocketAddress> inLinks) {
        this.inLinks = inLinks;
    }

    public int getNumNoneP2PConnections() {
        return numNoneP2PConnections;
    }

    public void setNumNoneP2PConnections(int numNoneP2PConnections) {
        this.numNoneP2PConnections = numNoneP2PConnections;
    }

    public int getMaxNumNoneP2PConnections() {
        return maxNumNoneP2PConnections;
    }

    public void setMaxNumNoneP2PConnections(int maxNumNoneP2PConnections) {
        this.maxNumNoneP2PConnections = maxNumNoneP2PConnections;
    }

    public List<InetSocketAddress> getKnownPeers() {
        return knownPeers;
    }

    public void setKnownPeers(List<InetSocketAddress> knownPeers) {
        this.knownPeers = knownPeers;
    }

    @Override
    public boolean equals(Object o) {

        // If the object is compared with itself then return true
        if (o == this) {
            return true;
        }

        /* Check if o is an instance of Complex or not
          "null instanceof [type]" also returns false */
        if (!(o instanceof P2PGreeting)) {
            return false;
        }

        // typecast o to P2PGreeting so that we can compare data members
        P2PGreeting c = (P2PGreeting) o;

        // Compare the data members and return accordingly
        return myMinimaPort == c.getMyMinimaPort()
            && isAcceptingInLinks == c.isAcceptingInLinks()
            && knownPeers.size() == c.getKnownPeers().size();
    }
}
