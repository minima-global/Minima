package org.minima.system.network.nio;

import java.net.Socket;
import java.util.ArrayList;

import org.minima.system.Main;
import org.minima.system.SystemHandler;
import org.minima.system.network.MultiServer;
import org.minima.system.network.NetClient;
import org.minima.utils.messages.Message;

public class NIONetworkHandler extends SystemHandler{

	public static final String NETWORK_STARTUP 		= "NETWORK_START";
	public static final String NETWORK_SHUTDOWN 	= "NETWORK_SHUTDOWN";
	
	public static final String NETWORK_CONNECT 		= "NETWORK_CONNECT";
	public static final String NETWORK_DISCONNECT 	= "NETWORK_DISCONNECT";
	public static final String NETWORK_RECONNECT 	= "NETWORK_RECONNECT";
	
	public static final String NETWORK_AUTOCONNECT 	= "NETWORK_AUTOCONNECT";
	
	public static final String NETWORK_NEWCLIENT 	= "NETWORK_NEWCLIENT";
	public static final String NETWORK_CLIENTERROR 	= "NETWORK_CLIENTERROR";
	
	public static final String NETWORK_PING 		= "NETWORK_PING";
	public static final String NETWORK_TRACE 		= "NETWORK_TRACE";
	
	public static final String NETWORK_SENDALL 		= "NETWORK_SENDALL";
	public static final String NETWORK_ALLSTOP 		= "NETWORK_ALLSTOP";
	
	
	/**
	 * The  server listening for clients..
	 */
	NIOMultiServer mServer;
	
	//All the network channels..
	ArrayList<NetClient> mClients 	= new ArrayList<>();
	
	/**
	 * 
	 * @param zMain
	 */
	public NIONetworkHandler(Main zMain) {
		super(zMain,"NIO_NETWORK");
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.isMessageType(NETWORK_STARTUP)) {
			//Get the port
			int port = zMessage.getInteger("port");
			
			//Start the network Server
			mServer = new NIOMultiServer();
			
			//And thread it..
			Thread tt = new Thread(mServer);
			tt.start();
			
		}else if(zMessage.isMessageType(NETWORK_SHUTDOWN)) {
			//Stop the server
			mServer.PostMessage(NIOMultiServer.NIOSERVER_CLOSE);
			mServer.stopMessageProcessor();
			
			//Shutdown all the clients
			Message msg = new Message(NetClient.NETCLIENT_SHUTDOWN);
			for(NetClient client : mClients) {
				client.PostMessage(msg);
			}
			
			//And finish up..
			stopMessageProcessor();
		
//		}else if(zMessage.isMessageType(NETWORK_RECONNECT)) {
//		}else if(zMessage.isMessageType(NETWORK_AUTOCONNECT)) {
//			//Send a TimedMessage..
//			Message connect  = new Message(NIONetworkHandler.NETWORK_CONNECT)
//				.addInt("port", 80)
//				.addString("host", "ec2-35-178-239-187.eu-west-2.compute.amazonaws.com");
//			
//			PostMessage(connect);
//			
		}else if(zMessage.isMessageType(NETWORK_CONNECT)) {
			String host = zMessage.getString("host");
			int port 	= zMessage.getInteger("port");
			
			//Now connect to it..
			Socket sock = new Socket(host, port);
			
			//Create a NetClient
//			NetClient client = new NetClient(sock, this);
			
			//Store with the rest
//			PostMessage(new Message(NETWORK_NEWCLIENT).addObject("client", client));
		
		}else if(zMessage.isMessageType(NETWORK_DISCONNECT)) {
			String uid = zMessage.getString("uid");
			
			for(NetClient client : mClients) {
				if(client.getUID().equals(uid)) {
					client.PostMessage(NetClient.NETCLIENT_SHUTDOWN);
					break;
				}
			}
			
		}else if(zMessage.isMessageType(NETWORK_NEWCLIENT)) {
			//get the client
			NetClient client = (NetClient)zMessage.getObject("client");
			
			//Add it
			mClients.add(client);
			
		}else if(zMessage.isMessageType(NETWORK_CLIENTERROR)) {
			//get the client
			NetClient client = (NetClient)zMessage.getObject("client");
			
			//Remove him from our list..
			mClients.remove(client);
		
			//Shut him down..
			client.PostMessage(new Message(NetClient.NETCLIENT_SHUTDOWN));
			
		}else if(zMessage.isMessageType(NETWORK_TRACE)) {
			boolean traceon = zMessage.getBoolean("trace");
			
			setLOG(traceon);
			
			for(NetClient client : mClients) {
				client.setLOG(traceon);
			}
		
		}else if(zMessage.isMessageType(NETWORK_SENDALL)) {
			//Get the message to send
			Message msg = (Message)zMessage.getObject("message");
			
			//Post it to all the clients
			sendToAll(msg);
		}
	}
	
	private void sendToAll(Message zMessage) {
		//Send to all the clients..
		for(NetClient client : mClients) {
			client.PostMessage(zMessage);
		}
	}
	
	public ArrayList<NetClient> getNetClients() {
		return mClients;
	}
	
}
