package org.minima.system.network.base;

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.Random;

import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.greet.Greeting;
import org.minima.objects.greet.SyncPackage;
import org.minima.objects.greet.TxPoWList;
import org.minima.system.Main;
import org.minima.system.brains.ConsensusNet;
import org.minima.system.network.NetworkHandler;
import org.minima.system.network.p2p.P2PMessageProcessor;
import org.minima.system.network.p2p.event.EventPublisher;
import org.minima.system.network.p2p.messages.*;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;

public class MinimaClient extends MessageProcessor {
		
	/**
	 * 5 muinutes before you can request the same TXPOW again..
	 */
	public static final long TXPOWRESQUEST_TIMEOUT = 1000 * 60 * 5;
	
	/**
	 * NetClient Messages
	 */
	public static final String NETCLIENT_INITCONNECT 	= "NETCLIENT_INITCONNECT";
	
	public static final String NETCLIENT_STARTUP 		= "NETCLIENT_STARTUP";
	public static final String NETCLIENT_SHUTDOWN 		= "NETCLIENT_SHUTDOWN";
		
	public static final String NETCLIENT_INTRO 	        = "NETCLIENT_INTRO";
	
	public static final String NETCLIENT_SENDTXPOWID 	= "NETCLIENT_SENDTXPOWID";
	
	public static final String NETCLIENT_SENDTXPOW 	    = "NETCLIENT_SENDTXPOW";
	public static final String NETCLIENT_SENDTXPOWREQ 	= "NETCLIENT_SENDTXPOWREQ";

	// Greeting is a return message
	public static final String NETCLIENT_GREETING 	    = "NETCLIENT_GREETING";

	// Sent to clients we have just connected too
	public static final String NETCLIENT_GREETING_REQ 	= "NETCLIENT_TXPOWLIST_REQ";
	
	public static final String NETCLIENT_TXPOWLIST 	    = "NETCLIENT_TXPOWLIST";
	public static final String NETCLIENT_TXPOWIDLIST    = "NETCLIENT_TXPOWIDLIST";
	
	public static final String NETCLIENT_PULSE 	        = "NETCLIENT_PULSE";
	public static final String NETCLIENT_PING 	        = "NETCLIENT_PING";

	public static final String NETCLIENT_P2P_RENDEZVOUS = "NETCLIENT_P2P_RENDEZVOUS";
	public static final String NETCLIENT_P2P_GREETING = "NETCLIENT_P2P_GREETING";
	public static final String NETCLIENT_P2P_WALK_LINKS = "NETCLIENT_P2P_WALK_LINKS";
	public static final String NETCLIENT_P2P_WALK_LINKS_RESPONSE = "NETCLIENT_P2P_WALK_LINKS_RESPONSE";
	public static final String NETMESSAGE_P2P_SWAP_LINK = "NETMESSAGE_P2P_SWAP_LINK";
	public static final String NETMESSAGE_P2P_DO_SWAP   = "NETMESSAGE_P2P_DO_SWAP";
	public static final String NETMESSAGE_P2P_MAP_NETWORK = "NETMESSAGE_P2P_MAP_NETWORK";
	public static final String NETMESSAGE_P2P_NODE_NOT_ACCEPTING = "NETMESSAGE_P2P_NODE_NOT_ACCEPTING";

	//Main Network Handler
	NetworkHandler mNetworkMain;
	
	//The socket
	Socket mSocket;
	
	//Output streams
	DataOutputStream mOutput;
	
	Thread 				mInputThread;
	MinimaReader		mInputReader;
	
	//The UID
	String mUID;
	
	//The Host and Port
	String mHost;
	int    mPort;
	InetSocketAddress address;
	InetSocketAddress minimaAddress;
	
	//Ping each other to know you are still up and running.. every 10 mins..
	public static final int PING_INTERVAL = 1000 * 60 * 10;
	long mLastPing = 0;
	
	/**
	 * If the connection breaks do we attempt to reconnect
	 */
	boolean mReconnect     = false;
	int mReconnectAttempts = 0;

	/**
	 * Incoming or Outgoing
	 */
	boolean mIncoming;

	/**
	 * Incoming or Outgoing
	 */
	boolean isClient = false;

	/**
	 * Constructor
	 * 
	 * @param zNetwork
	 * @throws IOException 
	 * @throws UnknownHostException 
	 */
	public MinimaClient(InetSocketAddress address, NetworkHandler zNetwork) {
		super("NETCLIENT");
		
		//Store
		mHost = address.getAddress().getHostAddress();
		mPort = address.getPort();
		this.address = address;
		this.minimaAddress = address;

		//We will attempt to reconnect if this connection breaks..
		mReconnect  = true;
		mReconnectAttempts = 0;
		
		mNetworkMain 	= zNetwork;
		
		//Create a UID
		mUID = ""+Math.abs(new Random().nextInt());

		//Outgoing connection
		mIncoming = false;
		
		//Start the connection
		PostMessage(NETCLIENT_INITCONNECT);
	}
	
	public MinimaClient(Socket zSock, NetworkHandler zNetwork) {
		super("NETCLIENT");
		
		//This is an incoming connection.. no reconnect attempt
		mReconnect = false;
		
		//Store
		mSocket 		= zSock;
		
		//Store
		mHost = mSocket.getInetAddress().getHostAddress();
		mPort = mSocket.getPort();
		address = new InetSocketAddress(mHost, mPort);
		
		//Main network Handler
		mNetworkMain 	= zNetwork;
				
		//Create a UID
		mUID = ""+Math.abs(new Random().nextInt());

		//Incoming connection
		mIncoming = true;

		//Start the system..
		PostMessage(NETCLIENT_STARTUP);
	}
	
	public Socket getSocket() {
		return mSocket;
	}
	
	public boolean isReconnect() {
		return mReconnect;
	}
	
	public void noReconnect() {
		mReconnect=false;
	}

	public InetSocketAddress getAddress() {
		return address;
	}

	public InetSocketAddress getMinimaAddress() {
		return minimaAddress;
	}
	public void setMinimaAddress(InetSocketAddress address) {
		this.minimaAddress = address;
	}

	public boolean isClient() {
		return isClient;
	}
	public void setIsClient(boolean isClient) {
		this.isClient = isClient;
	}

	public String getHost() {
		return mHost;
	}
	
	public int getPort() {
		return mPort;
	}
	
	public String getUID() {
		return mUID;
	}

	public boolean isIncoming() {
		return mIncoming;
	}
	
	public NetworkHandler getNetworkHandler() {
		return mNetworkMain;
	}
	
	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		
		ret.put("uid", mUID);
		ret.put("host", getHost());
		ret.put("port", getPort());
		ret.put("incoming", isIncoming());
		
		return ret;
	}
	
	@Override
	public String toString() {
		return toJSON().toString();
	}
	
	public void shutdown() {
		this.noReconnect();
		try {mOutput.close();}catch(Exception exc) {}
		try {mInputThread.interrupt();}catch(Exception exc) {}
		try {mSocket.close();}catch(Exception exc) {}
		
		stopMessageProcessor();
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.isMessageType(NETCLIENT_INITCONNECT)) {
			try {
				mSocket = new Socket();
				
				//Connect with timeout
				mSocket.connect(this.address, 60000);

				
			}catch (Exception e) {
				MinimaLogger.log("Error @ connection start : "+mHost+":"+mPort+" "+e);
				
				// Error - let the handler know
				mNetworkMain.PostMessage(new Message(NetworkHandler.NETWORK_CLIENTERROR).addObject("client", this));
				
				return;
			}

			//Start the system..
			PostMessage(NETCLIENT_STARTUP);
			
		}else if(zMessage.isMessageType(NETCLIENT_STARTUP)) {
			
			//Create the streams on this thread
			mOutput 	= new DataOutputStream(mSocket.getOutputStream());
			
			//Start reading
			mInputReader = new MinimaReader(this);
			mInputThread = new Thread(mInputReader, "NetClientReader");
			mInputThread.start();
			
			//First thing to do..
			PostMessage(new Message(NETCLIENT_P2P_GREETING));


			Message init = new Message(ConsensusNet.CONSENSUS_NET_INITIALISE);
			init.addObject("netclient", this);
			Main.getMainHandler().getConsensusHandler().PostMessage(init);
			
			//Latest communication..
			mLastPing = System.currentTimeMillis();
			
			//Send it again in a while..
			PostMessage(new Message(NETCLIENT_PULSE));
		
		}else if(zMessage.isMessageType(NETCLIENT_GREETING)) {
			Greeting greet = (Greeting)zMessage.getObject("greeting");
			sendMessage(MinimaReader.MsgType.NETMESSAGE_GREETING, greet);
		}else if(zMessage.isMessageType(NETCLIENT_TXPOWLIST)) {
			TxPoWList txplist = (TxPoWList)zMessage.getObject("txpowlist");
			sendMessage(MinimaReader.MsgType.NETMESSAGE_TXPOWLIST, txplist);
			
		}else if(zMessage.isMessageType(NETCLIENT_INTRO)) {
			SyncPackage sp = (SyncPackage)zMessage.getObject("syncpackage");
			sendMessage(MinimaReader.MsgType.NETMESSAGE_INTRO, sp);
		
		}else if(zMessage.isMessageType(NETCLIENT_SENDTXPOWID)) {
			MiniData txpowid = (MiniData)zMessage.getObject("txpowid");
			sendMessage(MinimaReader.MsgType.NETMESSAGE_TXPOWID, txpowid);
				
		}else if(zMessage.isMessageType(NETCLIENT_SENDTXPOW)) {
			TxPoW txpow = (TxPoW)zMessage.getObject("txpow");
			sendMessage(MinimaReader.MsgType.NETMESSAGE_TXPOW, txpow);
				
		}else if(zMessage.isMessageType(NETCLIENT_SENDTXPOWREQ)) {
			//get the TxPOW
			MiniData txpowid = (MiniData)zMessage.getObject("txpowid");
			
			//Don't ask for 0x00..
			if(txpowid.isEqual(MiniData.ZERO_TXPOWID)) {
				//it's the genesis parent..
				return;
			}
			
			//Send it..
			sendMessage(MinimaReader.MsgType.NETMESSAGE_TXPOW_REQUEST, txpowid);
	
		}else if(zMessage.isMessageType(NETCLIENT_PULSE)) {
			//When was the last PING message..
			long timenow = System.currentTimeMillis();
			long diff    = timenow - mLastPing;
			if(diff > PING_INTERVAL*2) {
				//Disconnect - Reconnect
				MinimaLogger.log("PING NOT RECEIVED IN TIME @ "+mHost+":"+mPort);
			
				//Disconnect..
				PostMessage(new Message(MinimaClient.NETCLIENT_SHUTDOWN));
				return;
			}
			
			//Send a PULSE message..
			sendMessage(MinimaReader.MsgType.NETMESSAGE_PING, MiniByte.TRUE);
			
			//Send it again in a while..
			PostTimerMessage(new TimerMessage(PING_INTERVAL, NETCLIENT_PULSE));
		
		}else if(zMessage.isMessageType(NETCLIENT_PING)) {
			//Received a PING message - This connection must still be working!
			mLastPing = System.currentTimeMillis();
		
		}else if(zMessage.isMessageType(NETCLIENT_SHUTDOWN)) {
			MinimaLogger.log("Shutting Down Client: " + address);
			Message msg = new Message(P2PMessageProcessor.P2P_ON_DISCONNECTED);
			msg.addObject("client", this);
			msg.addString("uid", this.getUID());
			getNetworkHandler().getP2PMessageProcessor().PostMessage(msg);
			shutdown();
		}else if(zMessage.isMessageType(NETCLIENT_P2P_GREETING)) {
			P2PMsgGreeting data = new P2PMsgGreeting(getNetworkHandler().getP2PMessageProcessor().getState(), this);
			sendMessage(MinimaReader.MsgType.NETMESSAGE_P2P_GREETING, data);
		}else if(zMessage.isMessageType(NETCLIENT_P2P_RENDEZVOUS)) {
			P2PMsgRendezvous data = (P2PMsgRendezvous) zMessage.getObject("data");
			sendMessage(MinimaReader.MsgType.NETMESSAGE_P2P_RENDEZVOUS, data);
		}else if(zMessage.isMessageType(NETCLIENT_P2P_WALK_LINKS)) {
			P2PMsgWalkLinks data = (P2PMsgWalkLinks) zMessage.getObject("data");
			sendMessage(MinimaReader.MsgType.NETMESSAGE_P2P_WALK_LINKS, data);
		}else if(zMessage.isMessageType(NETCLIENT_P2P_WALK_LINKS_RESPONSE)) {
			P2PMsgWalkLinks data = (P2PMsgWalkLinks) zMessage.getObject("data");
			sendMessage(MinimaReader.MsgType.NETMESSAGE_P2P_WALK_LINKS_RESPONSE, data);
		}else if(zMessage.isMessageType(NETMESSAGE_P2P_SWAP_LINK)) {
			P2PMsgSwapLink data = (P2PMsgSwapLink) zMessage.getObject("data");
			sendMessage(MinimaReader.MsgType.NETMESSAGE_P2P_SWAP_LINK, data);
		}else if(zMessage.isMessageType(NETMESSAGE_P2P_DO_SWAP)) {
			P2PMsgDoSwap data = (P2PMsgDoSwap) zMessage.getObject("data");
			sendMessage(MinimaReader.MsgType.NETMESSAGE_P2P_DO_SWAP, data);
		} else if(zMessage.isMessageType(NETMESSAGE_P2P_MAP_NETWORK)){
			P2PMsgNode data = (P2PMsgNode) zMessage.getObject("data");
			sendMessage(MinimaReader.MsgType.NETMESSAGE_P2P_MAP_NETWORK, data);
		} else if(zMessage.isMessageType(NETMESSAGE_P2P_NODE_NOT_ACCEPTING)) {
			P2PMsgNodeNotAccepting data = (P2PMsgNodeNotAccepting) zMessage.getObject("data");
			sendMessage(MinimaReader.MsgType.NETMESSAGE_P2P_NODE_NOT_ACCEPTING, data);
		}
	}
	
	/**
	 * Send a message down the network
	 */
	protected void sendMessage(MinimaReader.MsgType zMessageType, Streamable zObject) {
		//Are we connected ...
		if(mOutput == null) {
			//No connection yet..
			return;
		}
		
		//Send it..
		try {
			//Create a Data Object 
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			DataOutputStream dos = new DataOutputStream(baos);
			
			//Now write the Data..
			zObject.writeDataStream(dos);
			dos.flush();
			
			//Get the data..
			byte[] data = baos.toByteArray();
			int len     = data.length; 
			
			//Now wrap the message as a MiniData
			MiniData complete = new MiniData(data);
			
			//First write the Message type..
			zMessageType.getMiniByte().writeDataStream(mOutput);
			
			//Now write out the Size..
			MiniNumber minlen = new MiniNumber(len);
			minlen.writeDataStream(mOutput);
			
			//Now write the complete package..
			complete.writeDataStream(mOutput);
			EventPublisher.publish("sent " + zMessageType.name());

			//Send..
			mOutput.flush();
			
			//Close the streams
			dos.close();
			baos.close();
			
		}catch(Exception ec) {
			MinimaLogger.log("Error sending message : "+zMessageType.toString(),ec);
			
			//Tell the network Handler
			PostMessage(new Message(MinimaClient.NETCLIENT_SHUTDOWN));
		}
	}	
}
