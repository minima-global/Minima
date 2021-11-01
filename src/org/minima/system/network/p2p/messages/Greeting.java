package org.minima.system.network.p2p.messages;

import org.minima.system.network.p2p.P2PState;
import org.minima.system.params.GeneralParams;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

import java.net.InetSocketAddress;
import java.util.ArrayList;

public class Greeting {

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
    private ArrayList<InetSocketAddress> outLinks;

    /**
     * List of connected InLinks
     */
    private ArrayList<InetSocketAddress> inLinks;

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
    private ArrayList<InetSocketAddress> knownPeers;

    public Greeting() {
    }

    public Greeting(P2PState state) {

        this.myMinimaPort = GeneralParams.MINIMA_PORT;
        this.isAcceptingInLinks = state.isAcceptingInLinks();
        this.outLinks = new ArrayList<>(state.getOutLinks().values());
        this.inLinks = new ArrayList<>(state.getInLinks().values());
        this.numNoneP2PConnections = state.getNoneP2PLinks().size();
        this.maxNumNoneP2PConnections = state.getMaxNumNoneP2PConnections();
        this.knownPeers = new ArrayList<>(state.getKnownPeers());
    }

    /**
     * Constructor for when the node can't accept inLinks
     */
    public Greeting(int myMinimaPort, ArrayList<InetSocketAddress> knownPeers) {
        this.myMinimaPort = myMinimaPort;
        this.knownPeers = knownPeers;
        this.isAcceptingInLinks = false;
    }

    /**
     * Constructor for a full P2P Node
     */
    public Greeting(int myMinimaPort, boolean isAcceptingInLinks, ArrayList<InetSocketAddress> outLinks, ArrayList<InetSocketAddress> inLinks, int numNoneP2PConnections, int maxNumNoneP2PConnections, ArrayList<InetSocketAddress> knownPeers) {
        this.myMinimaPort = myMinimaPort;
        this.isAcceptingInLinks = isAcceptingInLinks;
        this.outLinks = outLinks;
        this.inLinks = inLinks;
        this.numNoneP2PConnections = numNoneP2PConnections;
        this.maxNumNoneP2PConnections = maxNumNoneP2PConnections;
        this.knownPeers = knownPeers;
    }

    public static Greeting fromJSON(JSONObject jsonObject) {
        Greeting greeting = new Greeting();
        greeting.setMyMinimaPort(Math.toIntExact((long) jsonObject.getOrDefault("myMinimaPort", 0)));
        greeting.setAcceptingInLinks((boolean) jsonObject.getOrDefault("isAcceptingInLinks", false));
        greeting.setKnownPeers(InetSocketAddressIO.addressesJSONToList((JSONArray) jsonObject.getOrDefault("knownPeers", new JSONArray())));
        if (greeting.isAcceptingInLinks()) {
            greeting.setOutLinks(InetSocketAddressIO.addressesJSONToList((JSONArray) jsonObject.getOrDefault("outLinks", new JSONArray())));
            greeting.setInLinks(InetSocketAddressIO.addressesJSONToList((JSONArray) jsonObject.getOrDefault("inLinks", new JSONArray())));
            greeting.setNumNoneP2PConnections(Math.toIntExact((long) jsonObject.getOrDefault("numNoneP2PConnections", 0)));
            greeting.setMaxNumNoneP2PConnections(Math.toIntExact((long) jsonObject.getOrDefault("maxNumNoneP2PConnections", 0)));
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

    public ArrayList<InetSocketAddress> getOutLinks() {
        return outLinks;
    }

    public void setOutLinks(ArrayList<InetSocketAddress> outLinks) {
        this.outLinks = outLinks;
    }

    public ArrayList<InetSocketAddress> getInLinks() {
        return inLinks;
    }

    public void setInLinks(ArrayList<InetSocketAddress> inLinks) {
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

    public ArrayList<InetSocketAddress> getKnownPeers() {
        return knownPeers;
    }

    public void setKnownPeers(ArrayList<InetSocketAddress> knownPeers) {
        this.knownPeers = knownPeers;
    }
}
