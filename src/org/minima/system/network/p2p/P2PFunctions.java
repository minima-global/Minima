package org.minima.system.network.p2p;

import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.network.minima.NIOClientInfo;
import org.minima.system.network.minima.NIOManager;
import org.minima.system.network.minima.NIOMessage;
import org.minima.system.network.p2p.params.P2PParams;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class P2PFunctions {

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

        boolean doConnect = true;
        List<NIOClientInfo> clients = getAllConnections();
        for (NIOClientInfo client : clients) {
            if (!client.isConnected() && client.getHost().equals(zHost) && client.getPort() == zPort) {
                MinimaLogger.log("Check connect failed already attempting to connect too:" + zHost + ":" + zPort);
                doConnect = false;
            }
        }

        if (doConnect) {
            //Call the NIOManager
            connect(zHost, zPort);
            P2PFunctions.log_debug("[!] P2P requesting NIO connection to: " + zHost + ":" + zPort);
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

    /**
     * Send a message to a specific peer
     *
     * @throws IOException
     */
    public static void sendP2PMessage(String zUID, JSONObject zMessage) throws IOException {
        //Convert the message to a streamable..
        MiniString json = new MiniString(zMessage.toString());

        //And now forward..
        NIOManager.sendNetworkMessage(zUID, NIOMessage.MSG_P2P, json);
    }

    /**
     * Send ALL peers a message
     *
     * @throws IOException
     */
    public static void sendP2PMessageAll(JSONObject zMessage) throws IOException {
        sendP2PMessage("", zMessage);
    }
    public static void log_node_runner(String message) {
        if (P2PParams.LOG_LEVEL == Level.NODE_RUNNER_MSG) {
            MinimaLogger.log("[P2P] " + message);
        }

    }

    public static void log_info(String message) {
        if (P2PParams.LOG_LEVEL == Level.INFO || P2PParams.LOG_LEVEL == Level.DEBUG) {
            MinimaLogger.log("[I] " + message);
        }
    }

    public static void log_debug(String message) {
        if (P2PParams.LOG_LEVEL == Level.DEBUG ) {
            MinimaLogger.log("[D] " + message);
        }
    }


}
