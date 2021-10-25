package org.minima.system.network.base;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.IOException;
import java.net.SocketException;
import java.util.ArrayList;

import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.objects.greet.Greeting;
import org.minima.objects.greet.SyncPackage;
import org.minima.objects.greet.TxPoWList;
import org.minima.system.Main;
import org.minima.system.brains.ConsensusHandler;
import org.minima.system.brains.ConsensusNet;
import org.minima.system.network.NetworkHandler;
import org.minima.system.network.p2p.P2PMessageProcessor;
import org.minima.system.network.p2p.messages.P2PMsgDoSwap;
import org.minima.system.network.p2p.messages.P2PMsgGreeting;
import org.minima.system.network.p2p.messages.P2PMsgNode;
import org.minima.system.network.p2p.messages.P2PMsgNodeNotAccepting;
import org.minima.system.network.p2p.messages.P2PMsgRendezvous;
import org.minima.system.network.p2p.messages.P2PMsgSwapLink;
import org.minima.system.network.p2p.messages.P2PMsgWalkLinks;
import org.minima.system.txpow.TxPoWChecker;
import org.minima.utils.Crypto;
import org.minima.utils.MiniFormat;
import org.minima.utils.MinimaLogger;
import org.minima.utils.ProtocolException;
import org.minima.utils.messages.Message;

public class MinimaReader implements Runnable {

    /**
     * Maximum Message sizes..
     */

    //50 MB MAX INTRO / Greeting / and TxPoW List
    public static final int MAX_INTRO = 1024 * 1000 * 50;

    //20 KB MAX MESSAGE
    public static final int MAX_TXPOW = 1024 * 20;

    //The Length of a TxPoWID message 64 + 4 byte int
    public static final int TXPOWID_LEN = Crypto.MINIMA_MAX_HASH_LENGTH + 4;

    /**
     * Greeting message that tells what Net Protocol this peer speaks, and a complete block chain header list. Any Blocks
     * the peer doesn't have he can request. Both peers send this to each other when they connect.
     */
    public static final MiniByte NETMESSAGE_GREETING = new MiniByte(0);

    /**
     * If the peers don't intersect a complete Sync Package is sent in this
     */
    public static final MiniByte NETMESSAGE_INTRO = new MiniByte(1);

    /**
     * The peer has a new TXPOW. This is the ID. If the peer
     * doesn't have it already he will request it
     */
    public static final MiniByte NETMESSAGE_TXPOWID = new MiniByte(2);

    /**
     * Request the full details of a TXPOW. Either the complete
     * txpow, or just the MMR proofs and updates.
     */
    public static final MiniByte NETMESSAGE_TXPOW_REQUEST = new MiniByte(3);

    /**
     * Complete TXPOW. Only sent if the peer has requested it
     */
    public static final MiniByte NETMESSAGE_TXPOW = new MiniByte(4);

    /**
     * A list of TxPoW details
     */
    public static final MiniByte NETMESSAGE_TXPOWLIST = new MiniByte(5);

    /**
     * PING PONG
     */
    public static final MiniByte NETMESSAGE_PING = new MiniByte(6);

    /**
     * GENERIC NETWORK MESSAGE
     */
    public static final MiniByte NETMESSAGE_GENERIC = new MiniByte(7);


    public static final MiniByte NETMESSAGE_P2P_RENDEZVOUS = new MiniByte(8);
    public static final MiniByte NETMESSAGE_P2P_WALK_LINKS = new MiniByte(9);
    public static final MiniByte NETMESSAGE_P2P_WALK_LINKS_RESPONSE = new MiniByte(10);
    public static final MiniByte NETMESSAGE_P2P_SWAP_LINK = new MiniByte(11);
    public static final MiniByte NETMESSAGE_P2P_DO_SWAP = new MiniByte(12);
    public static final MiniByte NETMESSAGE_P2P_MAP_NETWORK = new MiniByte(13);
    public static final MiniByte NETMESSAGE_P2P_GREETING = new MiniByte(14);
    public static final MiniByte NETMESSAGE_P2P_NODE_NOT_ACCEPTING = new MiniByte(15);

    /**
     * Netclient owner
     */
    MinimaClient mNetClient;

    /**
     * Constructor
     *
     * @param zNetClient
     */
    public MinimaReader(MinimaClient zNetClient) {
        mNetClient = zNetClient;
    }

    public void notifyListeners(String zMessage) {
        Main.getMainHandler().getConsensusHandler().notifyInitialListeners(zMessage);
    }

    @Override
    public void run() {
        try {
            //Create an input stream
            DataInputStream mInput = new DataInputStream(new BufferedInputStream(mNetClient.getSocket().getInputStream()));

            //The message type
            MiniByte msgtype = new MiniByte();

            //The Consensus
            ConsensusHandler consensus = Main.getMainHandler().getConsensusHandler();

            while (true) {
//                try {
                    if (mInput.available() > 0) {
                        //What message type
                        msgtype = MiniByte.ReadFromStream(mInput);

                        //What length..
                        int len = MiniNumber.ReadFromStream(mInput).getAsInt();

                        //Check within acceptable parameters - this should be set in TxPoW header.. for now fixed
                        if (msgtype.isEqual(NETMESSAGE_TXPOWID) ||
                                msgtype.isEqual(NETMESSAGE_TXPOW_REQUEST)) {
                            if (len > TXPOWID_LEN) {
                                throw new ProtocolException("Receive Invalid Message length for TXPOWID type:" + msgtype + " len:" + len);
                            }
                        } else if (msgtype.isEqual(NETMESSAGE_INTRO) ||
                                msgtype.isEqual(NETMESSAGE_GREETING) ||
                                msgtype.isEqual(NETMESSAGE_TXPOWLIST)) {
                            if (len > MAX_INTRO) {
                                throw new ProtocolException("Receive Invalid Message length for TXPOW_INTRO type:" + msgtype + " len:" + len);
                            }
                        } else if (msgtype.isEqual(NETMESSAGE_TXPOW)) {
                            if (len > MAX_TXPOW) {
                                throw new ProtocolException("Receive Invalid Message length for TXPOW type:" + msgtype + " len:" + len);
                            }
                        } else if (msgtype.isEqual(NETMESSAGE_GENERIC)) {
                            if (len > MAX_TXPOW) {
                                throw new ProtocolException("Receive Invalid GENERIC Message length :" + len);
                            }
                        } else if (msgtype.isEqual(NETMESSAGE_PING)) {
                            if (len > 1) {
                                throw new ProtocolException("Receive Invalid Message length for PING message type:" + msgtype + " len:" + len);
                            }
                        }

                        //The FULL message
                        MiniData fullmsg = null;

                        //Is this the LARGE initial Intro message..
                        if (msgtype.isEqual(NETMESSAGE_INTRO)) {
                            //tell us how big the sync was..
                            String ibdsize = MiniFormat.formatSize(len);
                            MinimaLogger.log("Initial Sync Message : " + ibdsize);
                            notifyListeners("Initial Sync Message : " + ibdsize);

                            //This is a MiniData Structure..
                            int datalen = mInput.readInt();

                            //Buffer for reading
                            byte[] datarr = new byte[8096];

                            ByteArrayOutputStream baos = new ByteArrayOutputStream(datalen);
                            long tot = 0;
                            long lastnotify = -1;
                            while (tot < datalen) {
                                long remain = datalen - tot;
                                if (remain > 8096) {
                                    remain = 8096;
                                }

                                //Read in the data..
                                int read = mInput.read(datarr, 0, (int) remain);
                                baos.write(datarr, 0, read);
                                tot += read;

                                //What Percent Done..
                                long newnotify = (tot * 100) / datalen;
                                if (newnotify != lastnotify) {
                                    lastnotify = newnotify;
                                    notifyListeners("IBD download : " + lastnotify + "% of " + ibdsize);
//							MinimaLogger.log("IBD download : "+lastnotify+"% of "+ibdsize);
                                }
                            }
                            baos.flush();

                            //Create the MiniData..
                            fullmsg = new MiniData(baos.toByteArray());

                        } else {
                            //Now read in the full message
                            fullmsg = MiniData.ReadFromStream(mInput, len);
                        }

                        //Now convert to an
                        ByteArrayInputStream bais = new ByteArrayInputStream(fullmsg.getData());
                        DataInputStream inputstream = new DataInputStream(bais);

                        //New Message received
                        Message rec = new Message(ConsensusNet.CONSENSUS_PREFIX + "NET_MESSAGE_" + msgtype);

                        //Always add the client
                        rec.addObject("netclient", mNetClient);

                        //What kind of message is it..
                        if (msgtype.isEqual(NETMESSAGE_INTRO)) {
                            //Read in the SyncPackage
                            SyncPackage sp = new SyncPackage();
                            sp.readDataStream(inputstream);

                            //Add and send
                            rec.addObject("sync", sp);

                        } else if (msgtype.isEqual(NETMESSAGE_TXPOWID)) {
                            //Peer now has this TXPOW - if you don't you can request the full version
                            MiniData hash = MiniData.ReadFromStream(inputstream);

                            //Add this ID
                            rec.addObject("txpowid", hash);

                        } else if (msgtype.isEqual(NETMESSAGE_TXPOW)) {
                            //A complete TxPOW
                            TxPoW txpow = new TxPoW();
                            txpow.readDataStream(inputstream);

                            //Is it even a valid TxPOW..
                            if (!TxPoWChecker.basicTxPowChecks(txpow)) {
                                //Hmm. something wrong..
                                throw new ProtocolException("INVALID TxPoW received : closing connection..");
                            }

                            //Add this ID
                            rec.addObject("txpow", txpow);

                        } else if (msgtype.isEqual(NETMESSAGE_TXPOW_REQUEST)) {
                            //Requesting a TxPOW
                            MiniData hash = MiniData.ReadFromStream(inputstream);

                            //Add this ID
                            rec.addObject("txpowid", hash);

                        } else if (msgtype.isEqual(NETMESSAGE_GREETING)) {
                            notifyListeners("Greeting Received..");

                            String greetsize = MiniFormat.formatSize(len);
                            MinimaLogger.log("Greeting Message : " + greetsize);

                            //Get the Greeting
                            Greeting greet = Greeting.ReadFromStream(inputstream);

                            //Add this ID
                            rec.addObject("greeting", greet);

                        } else if (msgtype.isEqual(NETMESSAGE_TXPOWLIST)) {
                            //tell us how big the sync was..
                            MinimaLogger.log("TxPoW List Message : " + MiniFormat.formatSize(len));

                            //Get the List
                            TxPoWList txplist = new TxPoWList();
                            txplist.readDataStream(inputstream);

                            //Check them all..
                            ArrayList<TxPoW> txps = txplist.getList();
                            for (TxPoW txp : txps) {
                                if (!TxPoWChecker.basicTxPowChecks(txp)) {
                                    //Hmm. something wrong..
                                    throw new ProtocolException("INVALID TxPoW received in TxPoW List : closing connection..");
                                }
                            }

                            //Add this ID
                            rec.addObject("txpowlist", txplist);

                        } else if (msgtype.isEqual(NETMESSAGE_PING)) {
                            MiniByte mb = MiniByte.ReadFromStream(inputstream);

                            //Add this ID
                            rec.addObject("sent", mb);

                        } else if (msgtype.isEqual(NETMESSAGE_GENERIC)) {
                            MiniString msg = MiniString.ReadFromStream(inputstream);

                            //Add this ID
                            rec.addObject("generic", msg);

                        } else if (msgtype.isEqual(NETMESSAGE_P2P_RENDEZVOUS)) {
                            MinimaLogger.log("[P2P] NETMESSAGE_P2P_RENDEZVOUS");
                            P2PMsgRendezvous rendezvous = P2PMsgRendezvous.ReadFromStream(inputstream);
                            if (!mNetClient.getNetworkHandler().getP2PMessageProcessor().getState().isRendezvousComplete()) {
                                Message msg = new Message(P2PMessageProcessor.P2P_RENDEZVOUS)
                                        .addObject("rendezvous", rendezvous)
                                        .addObject("client", mNetClient);
                                mNetClient.getNetworkHandler().getP2PMessageProcessor().PostMessage(msg);
                            }
                        } else if (msgtype.isEqual(NETMESSAGE_P2P_GREETING)) {
                            P2PMsgGreeting data = P2PMsgGreeting.ReadFromStream(inputstream);
                            Message msg = new Message(P2PMessageProcessor.P2P_ON_GREETED)
                                    .addObject("data", data)
                                    .addObject("client", mNetClient);
                            mNetClient.getNetworkHandler().getP2PMessageProcessor().PostMessage(msg);
                        } else if (msgtype.isEqual(NETMESSAGE_P2P_WALK_LINKS)) {
                            P2PMsgWalkLinks msgWalkLinks = P2PMsgWalkLinks.ReadFromStream(inputstream);
                            Message msg = new Message(P2PMessageProcessor.P2P_WALK_LINKS)
                                    .addObject("data", msgWalkLinks);
                            mNetClient.getNetworkHandler().getP2PMessageProcessor().PostMessage(msg);
                        } else if (msgtype.isEqual(NETMESSAGE_P2P_WALK_LINKS_RESPONSE)) {
                            P2PMsgWalkLinks msgWalkLinks = P2PMsgWalkLinks.ReadFromStream(inputstream);
                            Message msg = new Message(P2PMessageProcessor.P2P_WALK_LINKS_RESPONSE)
                                    .addObject("data", msgWalkLinks);
                            mNetClient.getNetworkHandler().getP2PMessageProcessor().PostMessage(msg);
                        } else if (msgtype.isEqual(NETMESSAGE_P2P_SWAP_LINK)) {
                            P2PMsgSwapLink data = P2PMsgSwapLink.ReadFromStream(inputstream);
                            Message msg = new Message(P2PMessageProcessor.P2P_SWAP_LINK)
                                    .addObject("data", data);
                            mNetClient.getNetworkHandler().getP2PMessageProcessor().PostMessage(msg);
                        } else if (msgtype.isEqual(NETMESSAGE_P2P_DO_SWAP)) {
                            P2PMsgDoSwap data = P2PMsgDoSwap.ReadFromStream(inputstream);
                            Message msg = new Message(P2PMessageProcessor.P2P_DO_SWAP)
                                    .addObject("data", data)
                                    .addObject("client", mNetClient);
                            mNetClient.getNetworkHandler().getP2PMessageProcessor().PostMessage(msg);
                        } else if (msgtype.isEqual(NETMESSAGE_P2P_MAP_NETWORK)) {
                            P2PMsgNode data = P2PMsgNode.ReadFromStream(inputstream);
                            Message msg = new Message(P2PMessageProcessor.P2P_MAP_NETWORK)
                                    .addObject("data", data)
                                    .addObject("client", mNetClient);
                            mNetClient.getNetworkHandler().getP2PMessageProcessor().PostMessage(msg);
                        } else if (msgtype.isEqual(NETMESSAGE_P2P_NODE_NOT_ACCEPTING)) {
                            P2PMsgNodeNotAccepting data = P2PMsgNodeNotAccepting.ReadFromStream(inputstream);
                            Message msg = new Message(P2PMessageProcessor.P2P_NODE_NOT_ACCEPTING)
                                    .addObject("data", data);
                            mNetClient.getNetworkHandler().getP2PMessageProcessor().PostMessage(msg);
                        
                        }else {
                        	//Unknown Message..
                        	MinimaLogger.log("UNKNOWN MESSAGE MINIMAREADER.. "+msgtype);
                        }

                        //Clean up..
                        inputstream.close();
                        bais.close();

                        //Post it..
                        consensus.PostMessage(rec);
                    }
            }
                    
        }catch(SocketException exc) {
			//Network error.. reset and reconnect..
    			MinimaLogger.log("SocketException.. "+exc);
		}catch(IOException exc) {
			//Network error.. reset and reconnect..
    			MinimaLogger.log("IOEXC.. "+exc);
//    			exc.printStackTrace();
			
		}catch(ProtocolException exc) {
			MinimaLogger.log("PROTOCOL ERROR.. "+exc);
			MinimaLogger.log(exc);
			
		}catch(OutOfMemoryError exc) {
			MinimaLogger.log("MEMORY ERROR.. "+exc);
			exc.printStackTrace();
			
			//DRASTIC ACTION.. Use ONLY if bash script in place to restart on Exit
			//System.exit(99);
			
		}catch(Exception exc) {
			//General Exception	
			MinimaLogger.log("NETCLIENTREADER ERROR.. ");
			MinimaLogger.log(exc);
		}

        //Tell the network Handler
        mNetClient.getNetworkHandler().PostMessage(new Message(NetworkHandler.NETWORK_CLIENTERROR).addObject("client", mNetClient));
    }
}

