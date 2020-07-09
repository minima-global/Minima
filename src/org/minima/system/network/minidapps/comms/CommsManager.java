package org.minima.system.network.minidapps.comms;

import java.util.ArrayList;

import org.minima.utils.MinimaLogger;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;

public class CommsManager extends MessageProcessor {

	public static final String COMMS_INIT = "COMMS_INIT";
	
	public static final String COMMS_START = "COMMS_STARTSERVER";
	public static final String COMMS_STOP  = "COMMS_STOPSERVER";
	
	public static final String COMMS_BROADCAST  = "COMMS_BROADCAST";
	
	public static final String COMMS_CONNECT    = "COMMS_CONNECT";
	public static final String COMMS_DISCONNECT = "COMMS_DISCONNECT";
	
	public static final String COMMS_NEWCLIENT  = "COMMS_NEWCLIENT";
	
	public static final String COMMS_SHUTDOWN   = "COMMS_SHUTDOWN";
	
	ArrayList<CommsServer> mServers;
	ArrayList<CommsClient> mClients;
	
	public CommsManager() {
		super("COMMSMANAGER");
	
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
			client.shutdown();	
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
			
			//Add to our List
			mServers.add(server);
			
		}else if(zMessage.getMessageType().equals(COMMS_STOP)) {
			int port = zMessage.getInteger("port");
		
			//Stop that server
			CommsServer server = getServer(port);
			server.stop();
		
			//Remove from the list..
			mServers.remove(server);
			
		}else if(zMessage.getMessageType().equals(COMMS_CONNECT)) {
			String hostport = zMessage.getString("hostport");
			int index = hostport.indexOf(":");
			String host = hostport.substring(0, index);
			int port = Integer.parseInt(hostport.substring(index+1).trim());
			
			CommsClient client = new CommsClient(host, port, this);
		
		}else if(zMessage.getMessageType().equals(COMMS_DISCONNECT)) {
			String hostport = zMessage.getString("hostport");
			int index = hostport.indexOf(":");
			String host = hostport.substring(0, index);
			int port = Integer.parseInt(hostport.substring(index+1).trim());
			
			//Get the Client..
			
			
			
		}else if(zMessage.getMessageType().equals(COMMS_NEWCLIENT)) {
			//There's a new client connected to a comms server
			CommsClient client = (CommsClient) zMessage.getObject("client");
			
			//Add to our List..
			mClients.add(client);	
			
		}else if(zMessage.getMessageType().equals(COMMS_BROADCAST)) {
			String message = zMessage.getString("message");
			int port = zMessage.getInteger("port");
			
			for(CommsClient client : mClients) {
				if(client.isBroadCast() && client.getPort() == port) {
					client.postSend(message);
				}
			}
		}
	}

}
