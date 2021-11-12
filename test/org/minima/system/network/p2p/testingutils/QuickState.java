package org.minima.system.network.p2p.testingutils;

import org.minima.system.network.p2p.P2PState;

import java.net.InetSocketAddress;
import java.net.UnknownHostException;

public class QuickState {

    public static P2PState stateNoConnections(boolean hasMinimaAddress) throws UnknownHostException {
        P2PState state = new P2PState();
        state.getKnownPeers().addAll(QuickInetLists.generateInetSockAddrList("192.168.0.", 1, 9001));
        if (hasMinimaAddress) {
            state.setMyMinimaAddress("10.0.0.1");
            state.setHostSet(true);
        }
        return state;
    }

    public static P2PState stateOutLinksOnly(int numOutLinks) throws UnknownHostException {
        P2PState state = new P2PState();
        state.setMyMinimaAddress("10.0.0.1");
        state.setHostSet(true);
        state.setOutLinks(QuickInetLists.generateConnectionUIDMap("outLinkUID", "70.0.0.", numOutLinks, 0));
        state.getKnownPeers().addAll(QuickInetLists.generateInetSockAddrList("192.168.0.", 1, 9001));
        return state;
    }

    public static P2PState stateInLinksOnly(int numInLinks, int port) throws UnknownHostException {
        P2PState state = new P2PState();
        state.setMyMinimaAddress("10.0.0.1");
        state.setHostSet(true);
        state.setInLinks(QuickInetLists.generateConnectionUIDMap("inLinkUID", "60.0.0.", numInLinks, port));
        state.getKnownPeers().addAll(QuickInetLists.generateInetSockAddrList("60.0.0.", 1, 9001));
        return state;
    }

    public static P2PState stateInLinkKnownPeerOnly() throws UnknownHostException {
        P2PState state = new P2PState();
        state.setMyMinimaAddress("10.0.0.1");
        state.setHostSet(true);
        state.getKnownPeers().addAll(QuickInetLists.generateInetSockAddrList("60.0.0.", 1, 9001));
        state.getInLinks().put("knownPeer", (InetSocketAddress) state.getKnownPeers().toArray()[0]);
        return state;
    }

    public static P2PState stateInAndOutLinks(int numOutLinks, int numInLinks, int port) throws UnknownHostException {
        P2PState state = new P2PState();
        state.setMyMinimaAddress("10.0.0.1");
        state.setHostSet(true);
        state.setOutLinks(QuickInetLists.generateConnectionUIDMap("outLinkUID", "70.0.0.", numOutLinks, port));
        state.setInLinks(QuickInetLists.generateConnectionUIDMap("inLinkUID", "60.0.0.", numInLinks, port));
        state.getKnownPeers().addAll(QuickInetLists.generateInetSockAddrList("192.168.0.", 1, 9001));
        return state;
    }


    public static P2PState stateNoneP2PLink(int numLinks, int port) throws UnknownHostException {
        P2PState state = new P2PState();
        state.setMyMinimaAddress("10.0.0.1");
        state.setHostSet(true);
        state.setNoneP2PLinks(QuickInetLists.generateConnectionUIDMap("noneP2PUID", "10.0.1.", numLinks, port));
        state.getKnownPeers().addAll(QuickInetLists.generateInetSockAddrList("192.168.0.", 1, 9001));
        return state;
    }

    public static P2PState stateFullLinks(int numLinks, int numNotAcceptingLinks, int port) throws UnknownHostException {
        P2PState state = new P2PState();
        state.setMyMinimaAddress("10.0.0.1");
        state.setHostSet(true);
        state.setOutLinks(QuickInetLists.generateConnectionUIDMap("outLinkUID", "70.0.0.", numLinks, port));
        state.setInLinks(QuickInetLists.generateConnectionUIDMap("inLinkUID", "60.0.0.", numLinks, port));
        state.setNotAcceptingConnP2PLinks(QuickInetLists.generateConnectionUIDMap("notAcceptingConnUID", "50.0.1.", numNotAcceptingLinks, port));
        state.setNoneP2PLinks(QuickInetLists.generateConnectionUIDMap("noneP2PUID", "10.0.1.", numLinks, port));
        state.getKnownPeers().addAll(QuickInetLists.generateInetSockAddrList("192.168.0.", 1, 9001));
        return state;
    }


}
