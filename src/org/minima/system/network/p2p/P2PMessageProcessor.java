package org.minima.system.network.p2p;

import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.brains.BackupManager;
import org.minima.system.input.InputHandler;
import org.minima.system.network.NetworkHandler;
import org.minima.system.network.base.MinimaClient;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;

import java.io.File;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;


public class P2PMessageProcessor extends MessageProcessor {

    /**
     * P2P Functions..
     */

    public static final String P2P_INIT = "P2P_INIT";
    public static final String P2P_SHUTDOWN = "P2P_SHUTDOWN";
    public static final String P2P_HEARTBEAT = "P2P_HEARTBEAT";
    public static final String P2P_REPEATLOOP = "P2P_REPEATLOOP";
    public static final String P2P_RECMESSAGE = "P2P_RECMESSAGE";
    public static final String P2P_PEERINFO = "P2P_SHOWPEERLIST";

    private static final int REPEAT_LOOP_DELAY = 60000;
    private static final int HEARTBEAT_DELAY = 600_000;


    //The data store
    P2PManager p2pManager;
    private InetAddress hostIP;
    private final int minimaPort;

    public P2PMessageProcessor(String hostIP, int minimaPort) {
        super("P2PManager");

        try {
            this.hostIP = InetAddress.getByName(hostIP);
        } catch (UnknownHostException e) {
            MinimaLogger.log("Could not identify the local ip address: " + hostIP);
        }
        this.minimaPort = minimaPort;

        //Start the Ball rolling..
        PostMessage(P2P_INIT);
    }

    public void stop() {
        PostMessage(P2P_SHUTDOWN);
    }

    /**
     * You can use this to get your HOST/IP etc
     *
     * @return
     */
    protected NetworkHandler getNetworkHandler() {
        return Main.getMainHandler().getNetworkHandler();
    }

    /**
     * All the current connections
     *
     * @return
     */
    protected ArrayList<MinimaClient> getCurrentMinimaClients() {
        return getNetworkHandler().getNetClients();
    }

    protected void sendMessage(MinimaClient zClient, String zMessage) {
        Message sender = new Message(MinimaClient.NETCLIENT_PEERS);
        sender.addObject("peersinfo", new MiniString(zMessage));
        zClient.PostMessage(sender);
    }

    protected void sendMessageAll(String zMessage) {
        ArrayList<MinimaClient> allclient = getCurrentMinimaClients();
        for (MinimaClient client : allclient) {
            sendMessage(client, zMessage);
        }
    }

    /**
     * Routes messages to the correct processing function
     *
     * @param zMessage The Full Message
     * @throws Exception
     */
    @Override
    protected void processMessage(Message zMessage) throws Exception {

        switch (zMessage.getMessageType()) {
            case P2P_INIT:
                processP2P_INIT_Msg(zMessage);
                break;
            case P2P_SHUTDOWN:
                processP2P_SHUTDOWN_Msg(zMessage);
                break;
            case P2P_HEARTBEAT:
                processP2P_HEARTBEAT_Msg(zMessage);
                break;
            case P2P_REPEATLOOP:
                processP2P_REPEATLOOP_Msg(zMessage);
                break;
            case P2P_RECMESSAGE:
                processP2P_RECMESSAGE_Msg(zMessage);
                break;
            case P2P_PEERINFO:
                processP2P_PEERINFO_Msg(zMessage);
                break;
            default:
                break;
        }
    }

    private void processP2P_INIT_Msg(Message zMessage) {
        // ============================================= //
        // Send out handshake requests to all known nodes
        // ============================================= //

        //Get the BackupManager
        BackupManager backup = Main.getMainHandler().getBackupManager();
        File p2pDataFile = backup.getBackUpFile("p2pdata.json");

        this.p2pManager = new P2PManager(new InetSocketAddress(this.hostIP, this.minimaPort), p2pDataFile);

        //Start the P2P Checker Loop - every minute
        PostTimerMessage(new TimerMessage(REPEAT_LOOP_DELAY, P2P_REPEATLOOP));

        ArrayList<P2PHandshake> handshakes = this.p2pManager.GenHandshakeWithUnverifiedNodes();

        for (P2PHandshake handshake : handshakes) {
            // TODO: Serialise and send message
        }

    }

    private void processP2P_SHUTDOWN_Msg(Message zMessage) {
        // Make sure the node list is saved
        this.p2pManager.SaveNodeList();
        //And stop this Message Processor stack
        stopMessageProcessor();
    }

    private void processP2P_HEARTBEAT_Msg(Message zMessage) {
        // ============================================= //
        // Send out a heartbeat message to the specified node
        // Check node is valid first
        // Add another P2P_HEARTBEAT to the timer again
        // ============================================= //
        P2PHeartbeat heartbeat = (P2PHeartbeat) zMessage.getObject("heartbeat");
        P2PNode targetNode = this.p2pManager.getVerifiedP2PNodeMap().get(heartbeat.getTargetNode().getIPAddress());
        // If targetNode is in the verified node map we know its healthy
        if ((targetNode != null) && (targetNode.isConnectable())){
            // TODO:  Serialise and send message
            // Send another heartbeat in 10 mins
            TimerMessage msg = new TimerMessage(HEARTBEAT_DELAY, P2P_HEARTBEAT);
            msg.addObject("heartbeat", heartbeat);
            PostTimerMessage(msg);
        }
        // Else no need to send another heartbeat to that node
        // So we do nothing and don't push it back to the message queue

    }

    private void processP2P_REPEATLOOP_Msg(Message zMessage) {
        // ============================================= //
        // P2P Manager main process
        // Prunes dead nodes
        // Update connection list
        // ============================================= //

        this.p2pManager.Update(getNetworkHandler());

        PostTimerMessage(new TimerMessage(REPEAT_LOOP_DELAY, P2P_REPEATLOOP));
    }

    private void processP2P_RECMESSAGE_Msg(Message zMessage) {
        // TODO After UDPServer has been coded
        //Received a message over the network..
        MinimaClient client = (MinimaClient) zMessage.getObject("minimaclient");
        MiniString str = (MiniString) zMessage.getObject("peersinfo");

        //Do something.. if it's a JSON..
        //JSONObject json = (JSONObject) new JSONParser().parse(str.toString());

        MinimaLogger.log("REC Peer info Client " + client.toJSON() + " " + str.toString());
    }

    private void processP2P_PEERINFO_Msg(Message zMessage) {
        // TODO
        //Return info to the peerlist function.. from terminal command
        //You can use the 'network function to see a list of current peers
        //but this you can use to test/debug' - use 'p2pinfo'

        //This is generic way to respond to messages from terminal
        JSONObject resp = InputHandler.getResponseJSON(zMessage);

        //Add some details..
        resp.put("info", "Some info!");
        resp.put("moreinfo", "Some more info!");

        //status true!
        InputHandler.endResponse(zMessage, true, "");
    }

}
