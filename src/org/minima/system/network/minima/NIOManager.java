package org.minima.system.network.minima;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.nio.channels.SocketChannel;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.StringTokenizer;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.minima.database.MinimaDB;
import org.minima.objects.Greeting;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.system.Main;
import org.minima.system.commands.network.connect;
import org.minima.system.commands.network.sshtunnel;
import org.minima.system.network.NetworkManager;
import org.minima.system.network.maxima.MaximaManager;
import org.minima.system.network.p2p.P2PFunctions;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;

public class NIOManager extends MessageProcessor {

	public static final String NIO_SERVERSTARTED 	= "NIO_SERVERSTARTED";
	
	public static final String NIO_SHUTDOWN 		= "NIO_SHUTDOWN";
	
	public static final String NIO_CONNECT 			= "NIO_CONNECT";
	public static final String NIO_CONNECTATTEMPT 	= "NIO_CONNECTATTEMPT";
	public static final String NIO_NEWCONNECTION 	= "NIO_NEWCONNECT";
	
	public static final String NIO_DISCONNECT 		= "NIO_DISCONNECT";
	public static final String NIO_DISCONNECTED		= "NIO_DISCONNECTED";
	public static final String NIO_DISCONNECTALL 	= "NIO_DISCONNECTALL";
	
	public static final String NIO_RECONNECT 		= "NIO_RECONNECT";
	
	public static final String NIO_INCOMINGMSG 		= "NIO_NEWMSG";
	
	public static final String NIO_TXPOWREQ 		= "NIO_REQTXPOW";

	/**
	 * How many attempts to reconnect
	 */
	public int RECONNECT_ATTEMPTS = 3;
	
	/**
	 * Check every minute to see if you have had a message in the last 2 mins..
	 */
	public static final String NIO_CHECKLASTMSG 	= "NIO_CHECKLASTMSG";
	long LASTREAD_CHECKER 		= 1000 * 120;
	long MAX_LASTREAD_CHECKER 	= 1000 * 60 * 5;
	
	/**
	 * Nuclear check to see if the networking is behaving itself 
	 */
	public static final String NIO_HEALTHCHECK 	= "NIO_HEALTHCHECK";
	long NIO_HEALTHCHECK_TIMER 	= 1000 * 60 * 20;
	
	/**
	 * How long before a reconnect attempt
	 */
	static final long RECONNECT_TIMER = 30000;
	
	/**
	 * Main Network Manager
	 */
	NetworkManager mNetworkManager;
	
	/**
	 * The MAIN Minima Server
	 */
	private NIOServer mNIOServer;
	
	/**
	 * Clients we are trying to connect to
	 */
	private ConcurrentHashMap<String, NIOClient> mConnectingClients;
	
	/**
	 * The clients connected awaiting added to NIOServer pool
	 */
	Object mSyncObject;
	HashSet<String> mAwaitingConnect = new HashSet<String>();
	
	/**
	 * Thread pool to manage incoming messages
	 */
	ExecutorService THREAD_POOL = Executors.newFixedThreadPool(4);
	
	public NIOManager(NetworkManager zNetManager) {
		super("NIOMANAGER");
		
		mNetworkManager = zNetManager;
		
		mConnectingClients = new ConcurrentHashMap<>();
		
		//New NIOServer
		mNIOServer = new NIOServer(GeneralParams.MINIMA_PORT, this);
		Thread nio = new Thread(mNIOServer);
		nio.start();
	}
	
	public NIOServer getNIOServer() {
		return mNIOServer;
	}
	
	public int getNumberOfConnectedClients() {
		return mNIOServer.getNetClientSize();
	}
	
	public int getNumberOfConnnectingClients() {
		return mConnectingClients.size();
	}
	
	public ArrayList<NIOClientInfo> getAllConnectionInfo() {
		//A list of all the connections
		ArrayList<NIOClientInfo> connections = new ArrayList<>();
		
		//Who are we trying to connect to
		Enumeration<NIOClient> clients = mConnectingClients.elements();
		while(clients.hasMoreElements()) {
			NIOClient nc 		= clients.nextElement();
			NIOClientInfo ninfo = new NIOClientInfo(nc, false);
			connections.add(ninfo);
		}
		
		//Who are we connected to..
		ArrayList<NIOClient> conns = mNIOServer.getAllNIOClients();
		for(NIOClient conn : conns) {
			NIOClientInfo ninfo = new NIOClientInfo(conn, true);
			connections.add(ninfo);
		}
		
		return connections;
	}
	
	public NIOClient checkConnected(String zHost, boolean zOnlyConnected) {
		
		if(!zOnlyConnected) {
			//Who are we trying to connect to
			Enumeration<NIOClient> clients = mConnectingClients.elements();
			while(clients.hasMoreElements()) {
				NIOClient nc = clients.nextElement();
				if(zHost.equals(nc.getFullAddress())) {
					return nc;
				}
			}
		}
		
		//Who are we connected to..
		ArrayList<NIOClient> conns = mNIOServer.getAllNIOClients();
		for(NIOClient conn : conns) {
			if(zHost.equals(conn.getFullAddress())) {
				return conn;
			}
		}
		
		return null;
	}
	
	public NIOClient getMaximaUID(String zMaximaPubKey) {
		ArrayList<NIOClient> conns = mNIOServer.getAllNIOClients();
		for(NIOClient conn : conns) {
			if(conn.getMaximaIdent().equals(zMaximaPubKey)) {
				return conn;
			}
		}
		
		return null;
	}
	
	public NIOClient getNIOClient(String zHost) {
		ArrayList<NIOClient> conns = mNIOServer.getAllNIOClients();
		for(NIOClient conn : conns) {
			if(conn.getFullAddress().equals(zHost)) {
				return conn;
			}
		}
		
		return null;
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.getMessageType().equals(NIO_SERVERSTARTED)) {
			
			//Do we need to start up the SSHTunnel..
			if(MinimaDB.getDB().getUserDB().isSSHTunnelEnabled()){
				//Start the SSH Tunnel..
				sshtunnel.startSSHTunnel();
			
				//Wait a few seconds for it to work..
				Thread.sleep(5000);
			}
			
			//The NIOServer has started you can now start up the P2P and pre-connect list
			mNetworkManager.getP2PManager().PostMessage(P2PFunctions.P2P_INIT);
			
			//Any nodes to auto connect to.. comma separated list
			if(!GeneralParams.CONNECT_LIST.equals("")) {
				
				StringTokenizer strtok = new StringTokenizer(GeneralParams.CONNECT_LIST,",");
				while(strtok.hasMoreTokens()) {
					String host = strtok.nextToken().trim();
					
					//Create the connect message
					Message msg = connect.createConnectMessage(host);
					if(msg == null) {
						MinimaLogger.log("ERROR connect host specified incorrectly : "+host);
					}else {
						PostMessage(msg);
					}
				}
			}
			
			//Check how long since last connect for each client..
			PostTimerMessage(new TimerMessage(LASTREAD_CHECKER, NIO_CHECKLASTMSG));
			
			//DO a health check on the state of the networking
			//PostTimerMessage(new TimerMessage(NIO_HEALTHCHECK_TIMER, NIO_HEALTHCHECK));
			
		}else if(zMessage.getMessageType().equals(NIO_SHUTDOWN)) {
			
			//Stop the Thread pool
			THREAD_POOL.shutdownNow();
			
			//Shut down the NIO
			mNIOServer.shutdown();
			
			//Wait for it to stop..
			while(mNIOServer.isRunning()) {
				Thread.sleep(50);
			}
			
			//Stop this..
			stopMessageProcessor();
		
		}else if(zMessage.getMessageType().equals(NIO_CONNECT)) {
			//Start a new connection - Outgoing connection
			String host = zMessage.getString("host");
			int port 	= zMessage.getInteger("port");
			
			//Create a new NetworkClient
			NIOClient nc = new NIOClient(host, port);
			
			//Add to our list
			mConnectingClients.put(nc.getUID(), nc);
			
			//Now try and connect..
			PostMessage(new Message(NIO_CONNECTATTEMPT).addObject("client", nc));
		
		}else if(zMessage.getMessageType().equals(NIO_CONNECTATTEMPT)) {
			//Get the client..
			NIOClient nc = (NIOClient) zMessage.getObject("client");
			
			//Is it still in the list..
			if(!mConnectingClients.containsKey(nc.getUID())) {
				//Has been removed.. stop trying to connect
				return;
			}
			
//			//How many connections - if too many stop.. 
//			if(getNumberOfConnnectingClients() > 10) {
//				MinimaLogger.log("Too many 'connecting' attempts - not connecting to "+nc.getFullAddress());
//				mConnectingClients.remove(nc.getUID());
//				
//				//Tell Maxima just in case..
//				Message maxconn = new Message(MaximaManager.MAXIMA_DISCONNECTED);
//				maxconn.addObject("nioclient", nc);
//				maxconn.addBoolean("reconnect", false);
//				Main.getInstance().getMaxima().PostMessage(maxconn);
//				
//				return;
//			}
			
			//Connect in separate thread..
			connectAttempt(nc);
			
		}else if(zMessage.getMessageType().equals(NIO_RECONNECT)) {
			//Get the client..
			NIOClient nc = (NIOClient) zMessage.getObject("client");
			
			//Increase the connect attempts
			nc.incrementConnectAttempts();
			
			//Do we try to reconnect
			boolean reconnect = true;
			
			//Do we attempt a reconnect..
			if(nc.getConnectAttempts() > RECONNECT_ATTEMPTS) {
				//Do we have ANY connections at all..
				ArrayList<NIOClient> conns = mNIOServer.getAllNIOClients();
				if(conns.size()>0) {
					
					//No reconnect
					reconnect = false;
					
					//Tell the P2P..
					Message newconn = new Message(P2PFunctions.P2P_NOCONNECT);
					newconn.addObject("client", nc);
					newconn.addString("uid", nc.getUID());
					mNetworkManager.getP2PManager().PostMessage(newconn);
					
					MinimaLogger.log("INFO : "+nc.getUID()+" connection failed - no more reconnect attempts ");
					
				}else {
					MinimaLogger.log("INFO : "+nc.getUID()+" Resetting reconnect attempts (no other connections) for "+nc.getFullAddress());
					
					//reset connect attempts..
					nc.setConnectAttempts(1);
				}
			}
			
			//Try and reconnect
			if(reconnect) {
				
				//Try again..
				TimerMessage tmsg = new TimerMessage(RECONNECT_TIMER, NIO_CONNECTATTEMPT);
				tmsg.addObject("client", nc);
				NIOManager.this.PostTimerMessage(tmsg);
			
			}else{
				
				//We are no  longer attempting to connect
				mConnectingClients.remove(nc.getUID());
				
				//Tell MAXIMA
				Message maxconn = new Message(MaximaManager.MAXIMA_DISCONNECTED);
				maxconn.addObject("nioclient", nc);
				maxconn.addBoolean("reconnect", false);
				Main.getInstance().getMaxima().PostMessage(maxconn);
			}
		
		}else if(zMessage.getMessageType().equals(NIO_DISCONNECTALL)) {
			
			//Disconnect from all the clients..!
			
			Enumeration<NIOClient> clients = mConnectingClients.elements();
			while(clients.hasMoreElements()) {
				NIOClient nc = clients.nextElement();
				disconnect(nc.getUID());
			}
			
			ArrayList<NIOClient> conns = mNIOServer.getAllNIOClients();
			for(NIOClient conn : conns) {
				disconnect(conn.getUID());
			}
			
		}else if(zMessage.getMessageType().equals(NIO_DISCONNECT)) {
			//Get the UID
			String uid = zMessage.getString("uid");
			
			//Remove from connecting..
			mConnectingClients.remove(uid);
			
			//And the connected as well..
			mNIOServer.disconnect(uid);
			
		}else if(zMessage.getMessageType().equals(NIO_DISCONNECTED)) {
			
			//Which nioclient
			NIOClient nioc = (NIOClient)zMessage.getObject("client");
			
			//Do we reconnect
			boolean reconnect = false;
			if(zMessage.exists("reconnect")) {
				reconnect = zMessage.getBoolean("reconnect");
			}
			
			//Lost a connection
			if(reconnect && nioc.isOutgoing()) {
				String host = nioc.getHost();
				int port 	= nioc.getPort();
				
				//Create a new NetworkClient
				NIOClient nc = new NIOClient(host, port);
				
				//Add to our list
				mConnectingClients.put(nc.getUID(), nc);
				
				//Create a connect message
				TimerMessage tmsg = new TimerMessage(RECONNECT_TIMER, NIO_CONNECTATTEMPT);
				tmsg.addObject("client", nc);
				PostTimerMessage(tmsg);
			}
			
			//Tell the P2P..
			Message newconn = new Message(P2PFunctions.P2P_DISCONNECTED);
			newconn.addObject("nioclient", nioc);
			newconn.addString("uid", nioc.getUID());
			newconn.addBoolean("incoming", nioc.isIncoming());
			newconn.addBoolean("reconnect", reconnect);
			mNetworkManager.getP2PManager().PostMessage(newconn);

			//Tell MAXIMA
			Message maxconn = new Message(MaximaManager.MAXIMA_DISCONNECTED);
			maxconn.addObject("nioclient", nioc);
			maxconn.addBoolean("reconnect", reconnect);
			Main.getInstance().getMaxima().PostMessage(maxconn);
			
		}else if(zMessage.getMessageType().equals(NIO_NEWCONNECTION)) {
			//New connection.. 
			NIOClient nioc = (NIOClient)zMessage.getObject("client");
		
			//We are no  longer attempting to connect
			mConnectingClients.remove(nioc.getUID());
			
			//Is this an outgoing connection..
			if(!nioc.isIncoming()) {
				
				//Only Send this ONCE!
				if(!nioc.haveSentGreeting()) {
					nioc.setSentGreeting(true);	
				
					//Create the Greeting..
					Greeting greet = new Greeting().createGreeting();
					
					//And send it..
					NIOManager.sendNetworkMessage(nioc.getUID(), NIOMessage.MSG_GREETING, greet);
				}
			}
			
		}else if(zMessage.getMessageType().equals(NIO_INCOMINGMSG)) {
			//Who is it from
			String uid = zMessage.getString("uid");
			
			//What was the message
			MiniData data = (MiniData) zMessage.getObject("data");
			
			//Create a handler task
			NIOMessage niomsg = new NIOMessage(uid, data);
			niomsg.setTrace(isTrace());
			
			//Process it.. in a thread pool..
			THREAD_POOL.execute(niomsg);
		
		}else if(zMessage.getMessageType().equals(NIO_TXPOWREQ)) {
			
			//Get the TxPoWID
			String txpowid = zMessage.getString("txpowid");
			
			//Which client..
			String clientid = zMessage.getString("client");

			//Why
			String reason = zMessage.getString("reason"); 
			
			//Check if we have it..
			if(!MinimaDB.getDB().getTxPoWDB().exists(txpowid)) {
		
				//Notify..
				MinimaLogger.log("INFO : Requesting TxPoW "+txpowid+" from "+clientid+" : "+reason);
				
				//Now get it..
				sendNetworkMessage(clientid, NIOMessage.MSG_TXPOWREQ, new MiniData(txpowid));
			}
		
		}else if(zMessage.getMessageType().equals(NIO_CHECKLASTMSG)) {
			
			//Check how long since last connect
			long timenow = System.currentTimeMillis();
			
			//Cycle and see..
			ArrayList<NIOClient> conns = mNIOServer.getAllNIOClients();
			for(NIOClient conn : conns) {
			
				long diff = timenow - conn.getLastReadTime();
				if(diff > MAX_LASTREAD_CHECKER) {
					
					//Too long a delay..
					MinimaLogger.log("INFO : No recent message (5 mins) from "
							+conn.getUID()+" disconnect/reconnect incoming:"
							+conn.isIncoming()+" valid:"+conn.isValidGreeting()+" host:"+conn.getFullAddress());
					
					//Disconnect
					disconnect(conn.getUID());
					
					//And reconnect in 5 secs if outgoing.. incoming will reconnect anyway
					if(!conn.isIncoming() && conn.isValidGreeting()) {
						TimerMessage timedconnect = new TimerMessage(5000, NIO_CONNECT);
						timedconnect.addString("host", conn.getHost());
						timedconnect.addInteger("port", conn.getPort());
						PostTimerMessage(timedconnect);
					}
				}
			}
			
			//And Again..
			PostTimerMessage(new TimerMessage(LASTREAD_CHECKER, NIO_CHECKLASTMSG));
		
		}else if(zMessage.getMessageType().equals(NIO_HEALTHCHECK)) {
			
			//Check the number of Connecting Clients.. if too great.. restart the networking..
			if(getNumberOfConnnectingClients() > 20 ) {
				
				//Log..
				MinimaLogger.log("Too Many connecting clients "+getNumberOfConnectedClients()+".. restarting networking");
				
				//Something not right..
				Message netstart = new Message(Main.MAIN_NETRESTART);
				netstart.addBoolean("repeat", false);
				Main.getInstance().PostMessage(netstart);
				
			}else {
			
				//DO a health check on the state of the networking
				PostTimerMessage(new TimerMessage(NIO_HEALTHCHECK_TIMER, NIO_HEALTHCHECK));
			}
		}
	}
	
	/**
	 * Connect to a client.. in a separate thread so returns immediately
	 */
	private void connectAttempt(final NIOClient zNIOClient) {
		
		Runnable connector = new Runnable() {
			
			@Override
			public void run() {
			
				try {
					//Create the socket channel if possible..
					InetSocketAddress addr 	= new InetSocketAddress(zNIOClient.getHost(), zNIOClient.getPort());
					SocketChannel sc 		= SocketChannel.open(addr);
					
					//Remove from the connecting..
					NIOManager.this.mConnectingClients.remove(zNIOClient.getUID());
					
					//we connected.. 
					NIOManager.this.mNIOServer.regsiterNewSocket(sc);
					
				}catch(Exception exc) {
					//Try again in a minute..
//					MinimaLogger.log(zNIOClient.getUID()+" INFO : connecting attempt "+zNIOClient.getConnectAttempts()+" to "+zNIOClient.getHost()+":"+zNIOClient.getPort()+" "+exc.toString());
					
					//Do we try to reconnect
					Message reconn = new Message(NIO_RECONNECT);
					reconn.addObject("client", zNIOClient);
					NIOManager.this.PostMessage(reconn);
				}
			}
		};
		
		Thread tt = new Thread(connector);
		tt.start();
	}
	
	/**
	 * Disconnect a client
	 */
	public void disconnect(String zClientUID) {
		Message msg = new Message(NIOManager.NIO_DISCONNECT).addString("uid", zClientUID);
		PostMessage(msg);
	}
	
	/**
	 * Send network messages
	 */
	public static void sendNetworkMessageAll(MiniByte zType, Streamable zObject) throws IOException {
		sendNetworkMessage("", zType, zObject);
	}
	
	public static void sendNetworkMessage(String zUID, MiniByte zType, Streamable zObject) throws IOException {
		//Make sure not null
		if(zObject == null) {
			throw new IOException("Cannot sendNetworkMessage with a NULL Object");
		}
		
		//Create the network message
		MiniData niodata = createNIOMessage(zType, zObject);
		
		//For ALL or for ONE
		if(!zUID.equals("")) {
			//Send it..
			Main.getInstance().getNIOManager().getNIOServer().sendMessage(zUID,niodata);
		}else {
			//Send it..
			Main.getInstance().getNIOManager().getNIOServer().sendMessageAll(niodata);
		}
	}
	
	public static MiniData createNIOMessage(MiniByte zType, Streamable zObject) throws IOException {
		
		//Create a stream to write to
		ByteArrayOutputStream baos 	= new ByteArrayOutputStream();
		DataOutputStream dos 		= new DataOutputStream(baos);
		
		//write the type
		zType.writeDataStream(dos);
		
		//Write the Object
		zObject.writeDataStream(dos);
		
		//Flush it..
		dos.flush();
		
		//Convert to byte array
		byte[] bb = baos.toByteArray();
		
		//Close all..
		dos.close();
		baos.close();
		
		//request it..
		MiniData data = new MiniData(bb);
		
		return data;
	}
	
	/**
	 * A special PING message to  chjeck a valid connection..
	 */
	public static Greeting sendPingMessage(String zHost, int zPort) {
		
		Greeting greet = null;
		
		try {
			//Create the Network Message
			MiniData msg = NIOManager.createNIOMessage(NIOMessage.MSG_SINGLE_PING, MiniData.ZERO_TXPOWID);
			
			//Open the socket..
			Socket sock 			= new Socket(zHost, zPort);
			sock.setSoTimeout(20000);
			
			//Create the streams..
			OutputStream out 		= sock.getOutputStream();
			DataOutputStream dos 	= new DataOutputStream(out);
			
			InputStream in			= sock.getInputStream();
			DataInputStream dis 	= new DataInputStream(in);
			
			//Write the data
			msg.writeDataStream(dos);
			dos.flush();
			
			//Load the message
			MiniData resp = MiniData.ReadFromStream(dis);
			
			//Close the streams..
			dis.close();
			in.close();
			dos.close();
			out.close();
			
			//Convert
			ByteArrayInputStream bais 	= new ByteArrayInputStream(resp.getBytes());
			DataInputStream bdis 		= new DataInputStream(bais);

			//What Type..
			MiniByte type = MiniByte.ReadFromStream(bdis);
			
			//Load the greeting
			greet = Greeting.ReadFromStream(bdis);
			
			bdis.close();
			bais.close();
		
		}catch(Exception exc){
			greet = null;
			MinimaLogger.log("Error sending Single Ping message : "+exc.toString());
		}
		
		return greet;
	}
}
