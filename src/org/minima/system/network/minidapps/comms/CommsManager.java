package org.minima.system.network.minidapps.comms;

import java.util.ArrayList;

import org.minima.system.Main;
import org.minima.system.SystemHandler;
import org.minima.system.network.NetworkHandler;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class CommsManager extends SystemHandler {

	public static final String COMMS_INIT = "COMMS_INIT";
	
	public static final String COMMS_START       = "COMMS_STARTSERVER";
	public static final String COMMS_NEWSERVER   = "COMMS_NEWSERVER";
	public static final String COMMS_SERVERERROR = "COMMS_SERVERERROR";
	public static final String COMMS_STOP        = "COMMS_STOPSERVER";
	
	public static final String COMMS_BROADCAST  = "COMMS_BROADCAST";
	
	public static final String COMMS_CONNECT    = "COMMS_CONNECT";
	public static final String COMMS_DISCONNECT = "COMMS_DISCONNECT";
	
	public static final String COMMS_NEWCLIENT    = "COMMS_NEWCLIENT";
	public static final String COMMS_CLIENTERROR  = "COMMS_CLIENTERROR";
	
	public static final String COMMS_SHUTDOWN   = "COMMS_SHUTDOWN";
	
	ArrayList<CommsServer> mServers;
	ArrayList<CommsClient> mClients;
	
	public CommsManager(Main zMain) {
		super(zMain, "COMMSMANAGER");
	
		mServers = new ArrayList<>();
		mClients = new ArrayList<>();
		
		PostMessage(COMMS_INIT);
	}
	
	public CommsServer getServer(int zPort) {
		for(CommsServer server : mServers) {
			if(server.getPort() == zPort) {
				return server;
			}
		}
		
		return null;
	}
	
	public CommsClient getClient(String zHost, int zPort) {
		for(CommsClient client : mClients) {
			if(client.getPort() == zPort && client.getHost().equals(zHost)) {
				return client;		
			}
		}
	
		return null;
	}
	
	public void shutdown() {
		//Shut down the servers
		for(CommsServer server : mServers) {
			server.stop();	
		}
		
		//Shut down any clients
		for(CommsClient client : mClients) {
			client.PostMessage(CommsClient.COMMSCLIENT_SHUTDOWN);	
		}
		
		stopMessageProcessor();
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		MinimaLogger.log("CommsManager : "+zMessage);
		
		
		if(zMessage.getMessageType().equals(COMMS_INIT)) {
		
			
		}else if(zMessage.getMessageType().equals(COMMS_START)) {
			//the details
			String minidapp = zMessage.getString("minidappid");
			int port = zMessage.getInteger("port");
		
			//Now create one..
			CommsServer server = new CommsServer(port, this);
			
			MinimaLogger.log("Total Servers : "+mServers.size());
		
		}else if(zMessage.getMessageType().equals(COMMS_NEWSERVER)) {
			//Get the Server
			CommsServer server = (CommsServer) zMessage.getObject("server");
			
			//Add to our List
			mServers.add(server);
			
			//Broadcast..
			JSONObject netaction = new JSONObject();
			netaction.put("type", "server");
			netaction.put("action", "new");
			netaction.put("port", server.getPort());
			postCommsMssage(netaction);
			
		}else if(zMessage.getMessageType().equals(COMMS_SERVERERROR)) {
			//Get the Server
			CommsServer server = (CommsServer) zMessage.getObject("server");
			
			//Add to our List
			mServers.remove(server);
			
			//Broadcast..
			JSONObject netaction = new JSONObject();
			netaction.put("type", "server");
			netaction.put("action", "error");
			netaction.put("port", server.getPort());
			netaction.put("error", zMessage.getString("error"));
			postCommsMssage(netaction);
			
		}else if(zMessage.getMessageType().equals(COMMS_STOP)) {
			int port = zMessage.getInteger("port");
		
			//Stop that server
			CommsServer server = getServer(port);
			if(server != null) {
				server.stop();	
			
				//Remove from the list..
				mServers.remove(server);
			
				//Broadcast..
				JSONObject netaction = new JSONObject();
				netaction.put("type", "server");
				netaction.put("action", "stop");
				netaction.put("port", server.getPort());
				postCommsMssage(netaction);
			}
			
		}else if(zMessage.getMessageType().equals(COMMS_CONNECT)) {
			String hostport = zMessage.getString("hostport");
			int index = hostport.indexOf(":");
			String host = hostport.substring(0, index);
			int port = Integer.parseInt(hostport.substring(index+1).trim());
			
			//Check for an old one..
			for(CommsClient client : mClients) {
				if(client.isOutBound()) {
					if(client.getHost().equals(host) && client.getPort() == port) {
						//Already connected..
						JSONObject netaction = new JSONObject();
						netaction.put("type", "client");
						netaction.put("action", "connection");
						netaction.put("host", client.getHost());
						netaction.put("port", client.getPort());
						netaction.put("error", "Allready conncted!");
						postCommsMssage(netaction);
						return;
					}
				}
			}
			
			//Start a new Client..
			CommsClient client = new CommsClient(host, port, this);
		
		}else if(zMessage.getMessageType().equals(COMMS_DISCONNECT)) {
			String hostport = zMessage.getString("hostport");
			int index = hostport.indexOf(":");
			String host = hostport.substring(0, index);
			int port = Integer.parseInt(hostport.substring(index+1).trim());
			
			//Get the Client..
			
		
		}else if(zMessage.getMessageType().equals(COMMS_CLIENTERROR)) {
			//There's a new client connected to a comms server
			CommsClient client = (CommsClient) zMessage.getObject("client");
			
			//Add to our List..
			mClients.remove(client);	
			
			JSONObject netaction = new JSONObject();
			netaction.put("type", "client");
			netaction.put("action", "error");
			netaction.put("host", client.getHost());
			netaction.put("port", client.getPort());
			netaction.put("error", zMessage.getString("error"));
			postCommsMssage(netaction);
			
			
		}else if(zMessage.getMessageType().equals(COMMS_NEWCLIENT)) {
			//There's a new client connected to a comms server
			CommsClient client = (CommsClient) zMessage.getObject("client");
			
			//Add to our List..
			mClients.add(client);	
			
			//Do we notify..
			JSONObject netaction = new JSONObject();
			if(client.isOutBound()) {
				netaction.put("type", "client");	
			}else {
				netaction.put("type", "server");	
			}
			netaction.put("action", "connection");
			netaction.put("host", client.getHost());
			netaction.put("port", client.getPort());
			netaction.put("uid", client.getUID());
			postCommsMssage(netaction);
			
		}else if(zMessage.getMessageType().equals(COMMS_BROADCAST)) {
			String message = zMessage.getString("message");
			int port = zMessage.getInteger("port");
			
			for(CommsClient client : mClients) {
				if(client.isInBound() && client.getPort() == port) {
					client.postSend(message);
				}
			}
		}
	}
	
	public void postCommsMssage(JSONObject zMessage) {
		//someone has connected to a port you opened..
		JSONObject newclient = new JSONObject();
		newclient.put("event","network");
		newclient.put("details",zMessage);
		
		Message msg = new Message(NetworkHandler.NETWORK_WS_NOTIFY);
		msg.addString("message", newclient.toString());
		
		//Post to the Network..
		getMainHandler().getNetworkHandler().PostMessage(msg);
	}

}
