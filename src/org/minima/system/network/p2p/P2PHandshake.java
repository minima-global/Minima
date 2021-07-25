package org.minima.system.network.p2p;

import java.net.InetAddress;
import java.util.ArrayList;
import java.util.List;

public class P2PHandshake {

    private final InetAddress targetIPAddress;
    private final int targetPort;
    private final InetAddress IPAddress;
    private final int port;
    private final int numP2PConnections;
    private final int numClientConnections;
    private final ArrayList<P2PNode> knownNodesList;

    public P2PHandshake(InetAddress targetIPAddress, int targetPort, InetAddress IPAddress, int port, int numP2PConnections, int numClientConnections, ArrayList<P2PNode> knownNodesList) {
        this.targetIPAddress = targetIPAddress;
        this.targetPort = targetPort;
        this.IPAddress = IPAddress;
        this.port = port;
        this.numP2PConnections = numP2PConnections;
        this.numClientConnections = numClientConnections;
        this.knownNodesList = knownNodesList;
    }

    public InetAddress getIPAddress() {
        return IPAddress;
    }

    public int getPort() {
        return port;
    }

    public int getNumP2PConnections() {
        return numP2PConnections;
    }

    public int getNumClientConnections() {
        return numClientConnections;
    }

    public ArrayList<P2PNode> getKnownNodesList() {
        return knownNodesList;
    }

    public int getTargetPort() {
        return targetPort;
    }

    public InetAddress getTargetIPAddress() {
        return targetIPAddress;
    }
}
