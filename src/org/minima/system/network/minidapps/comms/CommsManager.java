package org.minima.system.network.minidapps.comms;

import java.util.ArrayList;

import org.minima.system.Main;
import org.minima.system.SystemHandler;
import org.minima.system.input.InputHandler;
import org.minima.system.network.NetworkHandler;
import org.minima.system.network.minidapps.DAPPManager;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class CommsManager extends SystemHandler {

	public static final String COMMS_INIT = "COMMS_INIT";
	
	public static final String COMMS_START        = "COMMS_STARTSERVER";
	public static final String COMMS_NEWSERVER    = "COMMS_NEWSERVER";
	public static final String COMMS_SERVERERROR  = "COMMS_SERVERERROR";
	public static final String COMMS_STOP         = "COMMS_STOPSERVER";
	
	public static final String COMMS_BROADCAST    = "COMMS_BROADCAST";
	
	public static final String COMMS_CONNECT      = "COMMS_CONNECT";
	public static final String COMMS_DISCONNECT   = "COMMS_DISCONNECT";
	public static final String COMMS_SEND         = "COMMS_SEND";
	
	public static final String COMMS_NEWCLIENT    = "COMMS_NEWCLIENT";
	public static final String COMMS_CLIENTSHUT   = "COMMS_CLIENTSHUT";
	
	ArrayList<CommsServer> mServers;
	ArrayList<CommsClient> mClients;
	
	public CommsManager(Main zMain) {
		super(zMain, "COMMSMANAGER");
	
		mServers = new ArrayList<>();
		mClients = new ArrayList<>();
	}
	
	public ArrayList<CommsServer> getServers(){
		return mServers;
	}
	
	public ArrayList<CommsClient> getClients(){
		return mClients;
	}
	
	public CommsServer getServer(int zPort) {
		for(CommsServer server : mServers) {
			if(server.getPort() == zPort) {
				return server;
			}
		}
		
		return null;
	}
	
	public CommsClient getClient(String zUID) {
		for(CommsClient client : mClients) {
			if(client.getUID().equals(zUID)) {
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
		
		if(zMessage.getMessageType().equals(COMMS_START)) {
			//the details
			String minidapp = zMessage.getString("minidappid");
			int port = zMessage.getInteger("port");
		
			//Now create one..
			CommsServer server = new CommsServer(port, minidapp, this);
			
		}else if(zMessage.getMessageType().equals(COMMS_NEWSERVER)) {
			//Get the Server
			CommsServer server = (CommsServer) zMessage.getObject("server");
			
			//Add to our List
			mServers.add(server);
			
			//Broadcast..
			JSONObject netaction = new JSONObject();
			netaction.put("action", "server_start");
			netaction.put("port", server.getPort());
			postCommsMssage(netaction,zMessage.getString("minidappid"));
			
		}else if(zMessage.getMessageType().equals(COMMS_SERVERERROR)) {
			//Get the Server
			CommsServer server = (CommsServer) zMessage.getObject("server");
			
			//Add to our List
			mServers.remove(server);
			
			//Broadcast..
			JSONObject netaction = new JSONObject();
			netaction.put("action", "server_error");
			netaction.put("port", server.getPort());
			netaction.put("error", zMessage.getString("error"));
			postCommsMssage(netaction,zMessage.getString("minidappid"));
			
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
				netaction.put("action", "server_stop");
				netaction.put("port", port);
				postCommsMssage(netaction,zMessage.getString("minidappid"));
				
				//And stop the clients..
				for(CommsClient client : mClients) {
					if(client.getPort() == port && client.isInBound()) {
						client.PostMessage(CommsClient.COMMSCLIENT_SHUTDOWN);
					}
				}
			}else {
				//Broadcast..
				JSONObject netaction = new JSONObject();
				netaction.put("action", "server_notfound");
				netaction.put("port", port);
				postCommsMssage(netaction,zMessage.getString("minidappid"));
			}
			
		}else if(zMessage.getMessageType().equals(COMMS_CONNECT)) {
			String hostport = zMessage.getString("hostport");
			int index = hostport.indexOf(":");
			if(index == -1) {
				//Broadcast..
				JSONObject netaction = new JSONObject();
				netaction.put("action", "error");
				netaction.put("message", "inavlid host:port to connect to "+hostport);
				postCommsMssage(netaction,zMessage.getString("minidappid"));
				return;
			}
			
			String host = hostport.substring(0, index);
			int port = Integer.parseInt(hostport.substring(index+1).trim());
			
			//Check for an old one..
			for(CommsClient client : mClients) {
				if(client.isOutBound()) {
					if(client.getHost().equals(host) && client.getPort() == port) {
						//Already connected..
						return;
					}
				}
			}
			
			//Start a new Client..
			CommsClient client = new CommsClient(host, port, zMessage.getString("minidappid"), this);
		
		}else if(zMessage.getMessageType().equals(COMMS_DISCONNECT)) {
			String uid = zMessage.getString("uid");
			
			//Get the Client..
			for(CommsClient client : mClients) {
				if(client.getUID().equals(uid)) {
					client.PostMessage(CommsClient.COMMSCLIENT_SHUTDOWN);
				}
			}
		
		}else if(zMessage.getMessageType().equals(COMMS_CLIENTSHUT)) {
			//There's a new client connected to a comms server
			CommsClient client = (CommsClient) zMessage.getObject("client");
			
			//Add to our List..
			mClients.remove(client);	
			
			if(zMessage.exists("error")) {
				postClientMessage("client_shut",client, zMessage.getString("error"));
			}else {
				postClientMessage("client_shut",client);	
			}
			
		}else if(zMessage.getMessageType().equals(COMMS_NEWCLIENT)) {
			//There's a new client connected to a comms server
			CommsClient client = (CommsClient) zMessage.getObject("client");
			
			//Add to our List..
			mClients.add(client);	
			
			postClientMessage("client_new",client);
			
		}else if(zMessage.getMessageType().equals(COMMS_SEND)) {
			String uid     = zMessage.getString("uid");
			String message = zMessage.getString("message");
			
			CommsClient client = getClient(uid);
			if(client!= null) {
				client.postSend(message);	
			}
			
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
	
	public void postClientMessage(String zAction, CommsClient zClient) {
		postClientMessage(zAction, zClient, "");
	}
	
	public void postClientMessage(String zAction, CommsClient zClient, String zErorr) {
		//Already connected..
		JSONObject netaction = new JSONObject();
		netaction.put("action", zAction);
		
		netaction.put("host", zClient.getHost());
		netaction.put("port", zClient.getPort());
		netaction.put("uid", zClient.getUID());
		netaction.put("minidappid", zClient.getMiniDAPPID());
		netaction.put("outbound", zClient.isOutBound());
		
		if(!zErorr.equals("")) {
			netaction.put("error", zErorr);	
		}
		
		postCommsMssage(netaction, zClient.getMiniDAPPID());
	}
	
	public void postCommsMssage(JSONObject zMessage, String zMiniDAPPID) {
		//someone has connected to a port you opened..
		JSONObject websocketmsg = new JSONObject();
		websocketmsg.put("event","network");
		websocketmsg.put("details",zMessage);
		
		Message msg = new Message(DAPPManager.DAPP_MINIDAPP_POST);
		msg.addObject("message", websocketmsg);
		
		//Post to the Network..
		getMainHandler().getNetworkHandler().getDAPPManager().PostMessage(msg);
	}

}
