package org.minima.system.network.minidapps.comms;

import java.io.DataOutputStream;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.Random;

import org.minima.objects.base.MiniString;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;

public class CommsClient extends MessageProcessor {
	
	public static final String COMMSCLIENT_INIT     = "COMMSCLIENT_INIT";
	public static final String COMMSCLIENT_START    = "COMMSCLIENT_START";
	public static final String COMMSCLIENT_SHUTDOWN = "COMMSCLIENT_SHUTDOWN";
	
	public static final String COMMSCLIENT_MESSAGE = "COMMSCLIENT_MESSAGE";
	
	//The main internal comms hub
	CommsManager mCommsManager;
	
	//The socket
	Socket mSocket;
	
	//Output streams
	DataOutputStream mOutput;
	
	Thread 		    mInputThread;
	CommsReader		mInputReader;
	
	//The UID
	String mUID;
	
	//The Host and Port
	String mHost;
	int    mPort;
	
	/**
	 * Constructor
	 * 
	 * @param zSock
	 * @param zNetwork
	 * @throws IOException 
	 * @throws UnknownHostException 
	 */
	public CommsClient(String zHost, int zPort, CommsManager zCommsManager) {
		super("COMMSCLIENT");
		
		//Store
		mHost = zHost;
		mPort = zPort;
		
		mCommsManager = zCommsManager;
		
		//Create a UID
		mUID = ""+(new Random().nextLong());
		
		//Start the connection
		PostMessage(COMMSCLIENT_INIT);
	}
	
	public CommsClient(Socket zSock, CommsManager zCommsManager) {
		super("NETCLIENT");
		
		//Store
		mSocket 		= zSock;
		
		//Store
		mHost = mSocket.getInetAddress().getHostAddress();
		mPort = mSocket.getPort();
		
		mCommsManager = zCommsManager;
		
		//Create a UID
		mUID = ""+(new Random().nextLong());
		
		//Start the system..
		PostMessage(COMMSCLIENT_START);
	}
	
	public Socket getSocket() {
		return mSocket;
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
		
		if(zMessage.isMessageType(COMMSCLIENT_INIT)) {
			try {
				mSocket = new Socket();
				
				//Connect with timeout
				mSocket.connect(new InetSocketAddress(mHost, mPort), 60000);
				
			}catch (Exception e) {
				MinimaLogger.log("Error @ connection start : "+mHost+":"+mPort+" "+e);
				
				// Error - let the handler know
//				mNetworkMain.PostMessage(new Message(NetworkHandler.NETWORK_CLIENTERROR).addObject("client", this));
				
				return;
			}	
			
			//Start the system..
			PostMessage(COMMSCLIENT_START);
			
		}else if(zMessage.isMessageType(COMMSCLIENT_START)) {
			
			//Create the streams on this thread
			mOutput 	= new DataOutputStream(mSocket.getOutputStream());
			
			//Start reading
			mInputReader = new CommsReader(this);
			mInputThread = new Thread(mInputReader, "NetClientReader");
			mInputThread.start();
			
//			//First thing to do..
//			Message init = new Message(ConsensusNet.CONSENSUS_NET_INITIALISE);
//			init.addObject("netclient", this);
//			getMain().getConsensusHandler().PostMessage(init);
//			
//			//Latest communication..
//			mLastPing = System.currentTimeMillis();
//			
//			//Send it again in a while..
//			PostMessage(new Message(NETCLIENT_PULSE));
		
//		}else if(zMessage.isMessageType(NETCLIENT_PULSE)) {
//			//When was the last PING message..
//			long timenow = System.currentTimeMillis();
//			long diff    = timenow - mLastPing;
//			if(diff > PING_INTERVAL*2) {
//				//Disconnect - Reconnect
//				MinimaLogger.log("PING NOT RECEIVED IN TIME @ "+mHost+":"+mPort);
//			
//				//Disconnect..
//				PostMessage(new Message(CommsClient.NETCLIENT_SHUTDOWN));
//				return;
//			}
//			
//			//Send a PULSE message..
//			sendMessage(CommsReader.NETMESSAGE_PING, MiniByte.TRUE);
//			
//			//Send it again in a while..
//			PostTimerMessage(new TimerMessage(PING_INTERVAL, NETCLIENT_PULSE));
		
//		}else if(zMessage.isMessageType(NETCLIENT_PING)) {
//			//Received a PING message - This connection must still be working!
//			mLastPing = System.currentTimeMillis();
		
		}else if(zMessage.isMessageType(COMMSCLIENT_SHUTDOWN)) {
			
			try {mOutput.close();}catch(Exception exc) {}
			try {mInputThread.interrupt();}catch(Exception exc) {}
			try {mSocket.close();}catch(Exception exc) {}
			
			stopMessageProcessor();
		}
	}
	
	/**
	 * Send a message down the network
	 */
	protected void sendMessage(MiniString zMessage) {
		//Send it..
		try {
			//Write to Stream
			zMessage.writeDataStream(mOutput);
			
			//Pump it!
			mOutput.flush();
			
		}catch(Exception ec) {
			//Show..
			MinimaLogger.log("COMMS Error sending message : "+zMessage.toString()+" "+ec);
			
			//Tell the network Handler
			PostMessage(new Message(CommsClient.COMMSCLIENT_SHUTDOWN));
		}
	}	
}
