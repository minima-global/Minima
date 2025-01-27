package org.minima.system.network.p2p2;

import java.io.IOException;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.network.minima.NIOClientInfo;
import org.minima.system.network.minima.NIOManager;
import org.minima.system.network.minima.NIOMessage;
import org.minima.system.network.p2p.params.P2PParams;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class P2P2Functions {

    public enum Level {
        NODE_RUNNER_MSG, INFO, DEBUG,
    }

    /**
     * Default messages
     * <p>
     * Other messages can be sent of course
     */
    public static final String P2P_INIT = "P2P_INIT";
    public static final String P2P_SHUTDOWN = "P2P_SHUTDOWN";

    /**
     * Tells you the UID and if we attempt reconnect
     */
    public static final String P2P_CONNECTED = "P2P_CONNECTED";
    public static final String P2P_DISCONNECTED = "P2P_DISCONNECTED";

    /**
     * After many attempts this connection is not working..
     */
    public static final String P2P_NOCONNECT = "P2P_NOCONNECT";

    /**
     * P2P message sent from a peer
     */
    public static final String P2P_MESSAGE = "P2P_MESSAGE";

    /**
     * A list of Network Interfaces
     */
    private static Set<String> mLocalAddresses = null;
    public static Set<String> getLocalAddresses() {
    	if(mLocalAddresses == null) {
    		try {
				mLocalAddresses = getAllNetworkInterfaceAddresses();
			} catch (SocketException e) {
				mLocalAddresses = new HashSet<String>();
				mLocalAddresses.add("localhost");
				mLocalAddresses.add("127.0.0.1");
				mLocalAddresses.add("127.0.1.1");
				
			}
    	}
    	
    	return mLocalAddresses;
    }
    
    /**
     * An array of invalid IPs that you should not connect to..
     */
    public static HashSet<String> mInvalidPeers = new HashSet<>();
    public static void addInvalidPeer(String zHostPost) {
    	if(!mInvalidPeers.contains(zHostPost)) {
    		MinimaLogger.log("INVALID PEER added to List! "+zHostPost);
    		mInvalidPeers.add(zHostPost);
    	}else {
    		//MinimaLogger.log("INVALID PEER already added to list.. "+zHostPost);
    	}
    }
    
    public static boolean isInvalidPeer(String zHostPost) {
    	return mInvalidPeers.contains(zHostPost);
    }
    
    //Call this every 24 hours or so..
    public static void clearInvalidPeers() {
    	mInvalidPeers.clear();
    }
    
    public static boolean isIPv6(String fullhost) {
    	return 	fullhost.indexOf(":") != fullhost.lastIndexOf(":");
    }
    
    public static boolean isIPLocal(String fullhost) {
    	return 	fullhost.startsWith("127.")  ||
    			fullhost.startsWith("localhost") ||
				fullhost.startsWith("10.")   || 
				fullhost.startsWith("100.")  ||
				fullhost.startsWith("0.") 	 ||
				fullhost.startsWith("169.")  ||
				fullhost.startsWith("172.")  ||
				fullhost.startsWith("198.")  ||
				fullhost.startsWith("192.");
    }
    
    public static boolean isNetAvailable() {
        try {
            final URL url = new URL("https://www.google.com");
            final URLConnection conn = url.openConnection();
            conn.connect();
            conn.getInputStream().close();
            return true;
        } catch (Exception e) {
            //Could not connect 
        }
        return false;
    }
    
    /**
     * Connect to a Host and port if we don't already have a pending connection
     */
    public static void connect(String zHost, int zPort) {
        //Connect Message
        Message msg = new Message(NIOManager.NIO_CONNECT);
        msg.addString("host", zHost);
        msg.addInteger("port", zPort);

        //Call the NIOManager
        Main.getInstance().getNIOManager().PostMessage(msg);
    }

    public static boolean checkConnect(String zHost, int zPort) {
        
    	//Connect Message
        Message msg = new Message(NIOManager.NIO_CONNECT);
        msg.addString("host", zHost);
        msg.addInteger("port", zPort);

        //Check if added to naughty list
        if(isInvalidPeer(zHost+":"+zPort)) {
        	MinimaLogger.log("P2P2 CHECK CONNECT : Trying to connect to Invalid Peer - disallowed @ "+zHost+":"+zPort);
        	return false;
        }
        
        if(isIPv6(zHost+":"+zPort)) {
        	MinimaLogger.log("P2P2 CHECK CONNECT : Trying to connect to Invalid Ipv6 Peer - disallowed @ "+zHost+":"+zPort);
        	return false;
        }
        
        boolean doConnect = true;
        try {
            boolean islocal = isIPLocal(zHost);
            if (!GeneralParams.ALLOW_ALL_IP && islocal){
            	MinimaLogger.log("[!] P2P2 not connecting to local host : " + zHost + ":" + zPort);
                return false;
            }
            
        } catch (Exception e){
            MinimaLogger.log("[-] Error getting local addresses");
        }

        List<NIOClientInfo> clients = getAllConnections();
        for (NIOClientInfo client : clients) {
            if (!client.isConnected() && client.getHost().equals(zHost) && client.getPort() == zPort) {
                MinimaLogger.log("Check connect failed already attempting to connect too:" + zHost + ":" + zPort);
                return false;
            }
        }

        if (doConnect) {
            //Call the NIOManager
            connect(zHost, zPort);
            MinimaLogger.log("[!] P2P2 requesting NIO connection to: " + zHost + ":" + zPort);
        }
        
        return doConnect;
    }

    /**
     * Disconnect using the UID
     */
    public static void disconnect(String zUID) {
        Main.getInstance().getNIOManager().disconnect(zUID);
    }

    /**
     * Get ALL the current connections..
     * <p>
     * status shows connecting or connected..
     */
    public static ArrayList<NIOClientInfo> getAllConnections() {
        return Main.getInstance().getNetworkManager().getNIOManager().getAllConnectionInfo();
    }

    public static ArrayList<NIOClientInfo> getAllConnectedConnections() {
        ArrayList<NIOClientInfo> activeConnections = new ArrayList<>();
        for (NIOClientInfo nci: Main.getInstance().getNetworkManager().getNIOManager().getAllConnectionInfo()) {
            if (nci.isConnected()){
                activeConnections.add(nci);
            }
        }
        return activeConnections;
    }


    /**
     * Get a specific Client.. you can set and get extra data..
     */
    public static NIOClientInfo getNIOCLientInfo(String zUID) {
        ArrayList<NIOClientInfo> allclients = getAllConnections();

        for (NIOClientInfo info : allclients) {
            if (info.getUID().equals(zUID)) {
                return info;
            }
        }

        return null;
    }
    
    private static Set<String> getAllNetworkInterfaceAddresses() throws SocketException {
        Enumeration<NetworkInterface> nets = NetworkInterface.getNetworkInterfaces();
        Set<String> hostnames = new HashSet<>();
        for (NetworkInterface nif: Collections.list(nets)){
            Enumeration<InetAddress> addresses = nif.getInetAddresses();
            for (InetAddress address: Collections.list(addresses)) {
                hostnames.add(address.getHostName());
            }
        }
        return hostnames;
    }

    public static void main(String[] zArgs) {
    	System.out.println("NET:"+isNetAvailable());
    }
}
