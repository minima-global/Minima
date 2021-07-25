package org.minima.system.network.p2p;

import java.net.InetAddress;

public class P2PHeartbeat {
    private final InetAddress targetIPAddress;
    private final int targetPort;
    private final InetAddress IPAddress;
    private final int port;
    private final int numP2PConnections;
    private final int numClientConnections;

    public P2PHeartbeat(InetAddress targetIPAddress, int targetPort, InetAddress ipAddress, int port, int numP2PConnections, int numClientConnections) {
        this.targetIPAddress = targetIPAddress;
        this.targetPort = targetPort;
        IPAddress = ipAddress;
        this.port = port;
        this.numP2PConnections = numP2PConnections;
        this.numClientConnections = numClientConnections;
    }

    public InetAddress getTargetIPAddress() {
        return targetIPAddress;
    }

    public int getTargetPort() {
        return targetPort;
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
}
