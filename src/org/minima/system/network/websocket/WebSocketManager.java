package org.minima.system.network.websocket;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;

import org.minima.system.Main;
import org.minima.system.SystemHandler;
import org.minima.system.network.minidapps.DAPPManager;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.messages.Message;
import org.minima.utils.nanohttpd.protocols.websockets.CloseCode;

public class WebSocketManager extends SystemHandler {

	/**
	 * Message sent from the client..
	 */
	public static final String WEBSOCK_ONOPEN      = "WEBSOCK_ONOPEN";
	public static final String WEBSOCK_ONCLOSE     = "WEBSOCK_ONCLOSE";
	public static final String WEBSOCK_ONMESSAGE   = "WEBSOCK_ONMESSAGE";
	public static final String WEBSOCK_ONEXCEPTION = "WEBSOCK_ONEXCEPTION";
	
	public static final String WEBSOCK_SEND_INTRAMSG = "WEBSOCK_SEND_INTRAMSG";
	
	public static final String WEBSOCK_SEND        = "WEBSOCK_SEND";
	public static final String WEBSOCK_SENDTOALL   = "WEBSOCK_SENDTOALL";
	
	//The BASE WebSocketServer
	WebSocketServer mWebSockServer;
	
	//The List of all the currently connected MiniDAPPs..
	Hashtable<String, MinimaWebSocket> mMininaSockets;
	
	/**
	 * Main Constructor
	 * @param zMain
	 * @param zPort
	 * @throws IOException 
	 */
	public WebSocketManager(Main zMain, int zPort) throws IOException {
		super(zMain,"WEBSOCKETMANAGER");
		
		mMininaSockets = new Hashtable<>();
		
		//Start a Server
		try {
			mWebSockServer = new WebSocketServer(zPort, this);
			mWebSockServer.start(0,true);
		}catch(IOException exc) {
			MinimaLogger.log("WEBSOCKETMANAGER "+exc);
			mWebSockServer = null;
		}
	}

	public void stop() {
		//Close all the clients..
		Enumeration<MinimaWebSocket> clients = mMininaSockets.elements();
		while(clients.hasMoreElements()) {
			MinimaWebSocket client = clients.nextElement();
			try {
				client.close(CloseCode.NormalClosure, "System Shutdown", false);
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		
		//Stop the WebSocket Server
		if(mWebSockServer  != null) {
			mWebSockServer.stop();	
		}
		
		//Stop the message processor..
		stopMessageProcessor();
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		if(zMessage.getMessageType().equals(WEBSOCK_ONOPEN)) {
			//New WS Socket..
			MinimaWebSocket mws = (MinimaWebSocket) zMessage.getObject("wsclient");
			String UID = mws.getClientUID();
			
			//Add it to our list
			mMininaSockets.put(UID, mws);
			
		}else if(zMessage.getMessageType().equals(WEBSOCK_ONCLOSE)) {
			//New WS Socket..
			MinimaWebSocket mws = (MinimaWebSocket) zMessage.getObject("wsclient");
			String UID = mws.getClientUID();
			
			//Remove from our List
			mMininaSockets.remove(UID);
			
		}else if(zMessage.getMessageType().equals(WEBSOCK_ONEXCEPTION)) {
			//New WS Socket..
			MinimaWebSocket mws = (MinimaWebSocket) zMessage.getObject("wsclient");
			String UID = mws.getClientUID();
			
			//Remove from our List
			mMininaSockets.remove(UID);
		
		}else if(zMessage.getMessageType().equals(WEBSOCK_ONMESSAGE)) {
			//Get the client
			MinimaWebSocket mws = (MinimaWebSocket) zMessage.getObject("wsclient");
			String UID = mws.getClientUID();
			
			//Message is always in JSON format
			JSONObject msgobj = (JSONObject) new JSONParser().parse(zMessage.getString("message"));
			
			//What kind of message is it..
			String msgtype = (String) msgobj.get("type");
			if(msgtype.equals("uid")) {
				//Get the location..
				String loc = (String) msgobj.get("uid");
				
				//Default ID
				String mid = "0x00";
				
				//Get the MiniDAPPID
				int start = loc.indexOf("0x");
				if(start != -1) {
					int end = loc.indexOf("/",start);
					if(end != -1) {
						//Get it..!
						mid =  loc.substring(start,end);
					}else {
						mid =  loc.substring(start);
					}
				}
				
				//Set it..
				mws.setMiniDAPPUID(mid);
				
			}else if(msgtype.equals("reply")) {
				Message replymsg = new Message(DAPPManager.DAPP_DIRECTREPLY);
				replymsg.addString("replyid", (String)msgobj.get("replyid"));
				replymsg.addString("message", (String)msgobj.get("message"));
				
				//Send it to the DAPP MANAGER
				getMainHandler().getNetworkHandler().getDAPPManager().PostMessage(replymsg);
			}
			
		}else if(zMessage.getMessageType().equals(WEBSOCK_SEND)) {
			//Who to send the message to..
			String miniid = zMessage.getString("minidappid");
			
			//What to send..
			String msg = zMessage.getString("message");
			
			//Send a message to one of the listeners..
			Enumeration<MinimaWebSocket> clients = mMininaSockets.elements();
			while(clients.hasMoreElements()) {
				MinimaWebSocket client = clients.nextElement();
				if(client.getMiniDAPPUID().equals(miniid)) {
					try {
						//Try and send the message..
						client.send(msg);
						return;
					}catch(Exception exc){
						//Something wrong with this connection.. close..
						mMininaSockets.remove(client.getClientUID());
						break;
					}
				}
			}
			
			MinimaLogger.log("WS_SEND FRONTEND MINIDAPP NOT FOUND! "+zMessage);
			
		}else if(zMessage.getMessageType().equals(WEBSOCK_SENDTOALL)) {
			//What to send..
			String msg = zMessage.getString("message");
			
			//Get the errors
			ArrayList<String> remove = new ArrayList<>();
			
			Enumeration<MinimaWebSocket> clients = mMininaSockets.elements();
			while(clients.hasMoreElements()) {
				MinimaWebSocket client = clients.nextElement();
				try {
					//Try and send the message..
					client.send(msg);	
				}catch(Exception exc){
					//Something wrong with this connection.. close..
					remove.add(client.getClientUID());
				}
			}
			
			//Any errors..
			for(String errorclient : remove) {
				mMininaSockets.remove(errorclient);
			}
		}
	}	
}
