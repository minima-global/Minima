package org.minima.system.network.p2p;

import java.net.InetAddress;

public class P2PNode {
    
    private final InetAddress IPAddress;
    private final int port;
    private long lastSeenTimestamp;
    private int numActiveP2PConnections;
    private int numActiveClientConnections;
    private boolean isConnectedToNode;
    private boolean isConnectable;

    public P2PNode(InetAddress IPAddress, int port, long lastSeenTimestamp, int numActiveP2PConnections, int numActiveClientConnections, boolean isConnectedToNode, boolean isConnectable) {
        this.IPAddress = IPAddress;
        this.port = port;
        this.lastSeenTimestamp = lastSeenTimestamp;
        this.numActiveP2PConnections = numActiveP2PConnections;
        this.numActiveClientConnections = numActiveClientConnections;
        this.isConnectedToNode = isConnectedToNode;
        this.isConnectable = isConnectable;
    }


    public long getLastSeenTimestamp() {
        return lastSeenTimestamp;
    }

    public void setLastSeenTime(long lastSeenTimestamp) {
        this.lastSeenTimestamp = lastSeenTimestamp;
    }

    public int getNumActiveP2PConnections() {
        return numActiveP2PConnections;
    }

    public void setNumActiveP2PConnections(int numActiveP2PConnections) {
        this.numActiveP2PConnections = numActiveP2PConnections;
    }

    public int getNumActiveClientConnections() {
        return numActiveClientConnections;
    }

    public void setNumActiveClientConnections(int numActiveClientConnections) {
        this.numActiveClientConnections = numActiveClientConnections;
    }

    public boolean isConnectedToNode() {
        return isConnectedToNode;
    }

    public void setConnectedToNode(boolean connectedToNode) {
        isConnectedToNode = connectedToNode;
    }

    public boolean isConnectable() {
        return isConnectable;
    }

    public void setConnectable(boolean connectable) {
        isConnectable = connectable;
    }

    public InetAddress getIPAddress() {
        return IPAddress;
    }

    public int getPort() {
        return port;
    }
}
