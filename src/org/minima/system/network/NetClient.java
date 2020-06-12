package org.minima.system.network;

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Random;

import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.greet.Greeting;
import org.minima.objects.greet.HashNumber;
import org.minima.objects.greet.TxPoWList;
import org.minima.system.Main;
import org.minima.system.backup.SyncPackage;
import org.minima.system.brains.ConsensusNet;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;

public class NetClient extends MessageProcessor {
		
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
	
	public static final String NETCLIENT_GREETING 	    = "NETCLIENT_GREETING";
	public static final String NETCLIENT_TXPOWLIST_REQ 	= "NETCLIENT_TXPOWLIST_REQ";
	public static final String NETCLIENT_TXPOWLIST 	    = "NETCLIENT_TXPOWLIST";
	
	public static final String NETCLIENT_PULSE 	        = "NETCLIENT_PULSE";
	public static final String NETCLIENT_PING 	        = "NETCLIENT_PING";
	
	private static final MiniData ZERO_TXPOWID = new MiniData("0x00");
	
	//Main Network Handler
	NetworkHandler mNetworkMain;
	
	//The socket
	Socket mSocket;
	
	//Output streams
	DataOutputStream mOutput;
	
	Thread 				mInputThread;
	NetClientReader		mInputReader;
	
	//The UID
	String mUID;
	
	//The Host and Port
	String mHost;
	int    mPort;
	
	//Ping each other to know you are still up and running.. every 10 mins..
	public static final int PING_INTERVAL = 1000 * 60 * 10;
	long mLastPing = 0;
	
	/**
	 * If the connection breaks do we attempt to reconnect
	 */
	boolean mReconnect     = false;
	int mReconnectAttempts = 0;
	
	//Did we start up..
	boolean mStartOK;
	
	Hashtable<String, Long> mOldTxPoWRequests = new Hashtable<>();
	
	/**
	 * Constructor
	 * 
	 * @param zSock
	 * @param zNetwork
	 * @throws IOException 
	 * @throws UnknownHostException 
	 */
	public NetClient(String zHost, int zPort, NetworkHandler zNetwork) {
		super("NETCLIENT");
		
		//Store
		mHost = zHost;
		mPort = zPort;
		
		//We will attempt to reconnect if this connection breaks..
		mReconnect  = true;
		mReconnectAttempts = 0;
		
		mNetworkMain 	= zNetwork;
		
		//Create a UID
		mUID = ""+Math.abs(new Random().nextInt());
		
		//Start the connection
		PostMessage(NETCLIENT_INITCONNECT);
	}
	
	public NetClient(Socket zSock, NetworkHandler zNetwork) {
		super("NETCLIENT");
		
		//This is an incoming connection.. no reconnect attempt
		mReconnect = false;
		
		//Store
		mSocket 		= zSock;
		
		//Store
		mHost = mSocket.getInetAddress().getHostAddress();
		mPort = mSocket.getPort();
		
		//Main network Handler
		mNetworkMain 	= zNetwork;
				
		//Create a UID
		mUID = ""+Math.abs(new Random().nextInt());
		
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
	
	public String getHost() {
		return mHost;
	}
	
	public int getPort() {
		return mPort;
	}
	
	public String getUID() {
		return mUID;
	}
	
	public NetworkHandler getNetworkHandler() {
		return mNetworkMain;
	}
	
	private Main getMain() {
		return mNetworkMain.getMainHandler();
	}
	
	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		
		ret.put("uid", mUID);
		ret.put("host", getHost());
		ret.put("port", getPort());
		
		return ret;
	}
	
	@Override
	public String toString() {
		return toJSON().toString();
	}
	
	@Override
	public void stopMessageProcessor() {
		try {mOutput.close();}catch(Exception exc) {}
		try {mInputThread.interrupt();}catch(Exception exc) {}
		try {mSocket.close();}catch(Exception exc) {}
		
		super.stopMessageProcessor();
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.isMessageType(NETCLIENT_INITCONNECT)) {
			try {
				mSocket = new Socket();
				
				//Connect with timeout
				mSocket.connect(new InetSocketAddress(mHost, mPort), 60000);
				
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
			mInputReader = new NetClientReader(this);
			mInputThread = new Thread(mInputReader, "NetClientReader");
			mInputThread.start();
			
			//First thing to do..
			Message init = new Message(ConsensusNet.CONSENSUS_NET_INITIALISE);
			init.addObject("netclient", this);
			getMain().getConsensusHandler().PostMessage(init);
			
			//Latest communication..
			mLastPing = System.currentTimeMillis();
			
			//Send it again in a while..
			PostMessage(new Message(NETCLIENT_PULSE));
		
		}else if(zMessage.isMessageType(NETCLIENT_GREETING)) {
			Greeting greet = (Greeting)zMessage.getObject("greeting");
			sendMessage(NetClientReader.NETMESSAGE_GREETING, greet);
		
		}else if(zMessage.isMessageType(NETCLIENT_TXPOWLIST)) {
			TxPoWList txplist = (TxPoWList)zMessage.getObject("txpowlist");
			sendMessage(NetClientReader.NETMESSAGE_TXPOWLIST, txplist);
			
		}else if(zMessage.isMessageType(NETCLIENT_TXPOWLIST_REQ)) {
			HashNumber hn = (HashNumber)zMessage.getObject("hashnumber");
			sendMessage(NetClientReader.NETMESSAGE_TXPOWLIST_REQUEST, hn);
			
		}else if(zMessage.isMessageType(NETCLIENT_INTRO)) {
			SyncPackage sp = (SyncPackage)zMessage.getObject("syncpackage");
			sendMessage(NetClientReader.NETMESSAGE_INTRO, sp);
		
		}else if(zMessage.isMessageType(NETCLIENT_SENDTXPOWID)) {
			MiniData txpowid = (MiniData)zMessage.getObject("txpowid");
			sendMessage(NetClientReader.NETMESSAGE_TXPOWID, txpowid);
				
		}else if(zMessage.isMessageType(NETCLIENT_SENDTXPOW)) {
			TxPoW txpow = (TxPoW)zMessage.getObject("txpow");
			sendMessage(NetClientReader.NETMESSAGE_TXPOW, txpow);
				
		}else if(zMessage.isMessageType(NETCLIENT_SENDTXPOWREQ)) {
			//get the TxPOW
			MiniData txpowid = (MiniData)zMessage.getObject("txpowid");
			
			//Don't ask for 0x00..
			if(txpowid.isEqual(ZERO_TXPOWID)) {
				//it's the genesis..
				return;
			}
			
			//Current time..
			long timenow     = System.currentTimeMillis();
			
			//Remove the old..
			Hashtable<String, Long> newTxPoWRequests = new Hashtable<>();
			Enumeration<String> keys = mOldTxPoWRequests.keys();
			while(keys.hasMoreElements()) {
				String key = keys.nextElement();
				
				//Remove after 10 minutes
				Long timeval = mOldTxPoWRequests.get(key);
				long time    = timeval.longValue();
				long diff    = timenow - time;
				if(diff < (1000 * 60 * 10)) {
					newTxPoWRequests.put(key, timeval);
				}
			}
			
			//Swap them..
			mOldTxPoWRequests = newTxPoWRequests;
			
			//NOW - Check not doing it too often..
			String val = txpowid.to0xString();
			
			//If it's in.. it's less than 30 minutes..
			if(mOldTxPoWRequests.get(val) != null) {
				return;
			}
			
			//Store this as the LAST time we requested it.. won't do it again for 10 minutes
			mOldTxPoWRequests.put(val, new Long(timenow));
			
			//And send it..
			sendMessage(NetClientReader.NETMESSAGE_TXPOW_REQUEST, txpowid);
	
		}else if(zMessage.isMessageType(NETCLIENT_PULSE)) {
			//When was the last PING message..
			long timenow = System.currentTimeMillis();
			long diff    = timenow - mLastPing;
			if(diff > PING_INTERVAL*2) {
				//Disconnect - Reconnect
				MinimaLogger.log("PING NOT RECEIVED IN TIME @ "+mHost+":"+mPort);
			
				//Disconnect..
				PostMessage(new Message(NetClient.NETCLIENT_SHUTDOWN));
				return;
			}
			
			//Send a PULSE message..
			sendMessage(NetClientReader.NETMESSAGE_PING, MiniByte.TRUE);
			
			//Send it again in a while..
			PostTimerMessage(new TimerMessage(PING_INTERVAL, NETCLIENT_PULSE));
		
		}else if(zMessage.isMessageType(NETCLIENT_PING)) {
			//Received a PING message - This connection must still be working!
			mLastPing = System.currentTimeMillis();
		
		}else if(zMessage.isMessageType(NETCLIENT_SHUTDOWN)) {
			
			try {mOutput.close();}catch(Exception exc) {}
			try {mInputThread.interrupt();}catch(Exception exc) {}
			try {mSocket.close();}catch(Exception exc) {}
			
			stopMessageProcessor();
		}
	}
	
	/**
	 * Send a message down the network
	 */
	protected void sendMessage(MiniByte zMessageType, Streamable zObject) {
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
			zMessageType.writeDataStream(mOutput);
			
			//Now write out the Size..
			MiniNumber minlen = new MiniNumber(len);
			minlen.writeDataStream(mOutput);
			
			//Now write the complete package..
			complete.writeDataStream(mOutput);
			
			//Send..
			mOutput.flush();
			
			//Close the streams
			dos.close();
			baos.close();
			
		}catch(Exception ec) {
			//Show..
			MinimaLogger.log("Error sending message : "+zMessageType.toString()+" "+ec);
			
			//Tell the network Handler
			PostMessage(new Message(NetClient.NETCLIENT_SHUTDOWN));
		}
	}	
}
