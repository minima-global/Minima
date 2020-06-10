package org.minima.system.network.websocket;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;

import org.minima.system.Main;
import org.minima.system.SystemHandler;
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
	
	public static final String WEBSOCK_SENDTOALL   = "WEBSOCK_SENDTOALL";
	public static final String WEBSOCK_SEND_INTRAMSG        = "WEBSOCK_SEND";
	
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
				//Get the MiniDAPP UID
				String miniuid = (String) msgobj.get("uid");
				
				//Set it..
				mws.setMiniDAPPUID(miniuid);
				
			}else if(msgtype.equals("message")) {
				Message comms = new Message(WEBSOCK_SEND_INTRAMSG);
				comms.addString("event", "newmessage");
				comms.addString("from", mws.getMiniDAPPUID());
				comms.addString("uid", (String) msgobj.get("to"));
				comms.addString("message", (String) msgobj.get("message"));
				comms.addString("funcid", (String) msgobj.get("funcid"));
				
				PostMessage(comms);
			
			}else if(msgtype.equals("reply")) {
				Message comms = new Message(WEBSOCK_SEND_INTRAMSG);
				comms.addString("event", "newreply");
				comms.addString("from", mws.getMiniDAPPUID());
				comms.addString("uid", (String) msgobj.get("to"));
				comms.addString("message", (String) msgobj.get("message"));
				comms.addString("funcid", (String) msgobj.get("replyid"));
				
				PostMessage(comms);
			}
			
		}else if(zMessage.getMessageType().equals(WEBSOCK_SEND_INTRAMSG)) {
			//Who to..
			String event   = zMessage.getString("event");
			String uid     = zMessage.getString("uid");
			String from    = zMessage.getString("from");
			String message = zMessage.getString("message");
			String funcid  = zMessage.getString("funcid");
			
			JSONObject newmessage = new JSONObject();
			newmessage.put("event",event);
			newmessage.put("message",message);
			newmessage.put("functionid",funcid);
			newmessage.put("from",from);
			newmessage.put("error","");
			
			//Send a message to one of the listeners..
			Enumeration<MinimaWebSocket> clients = mMininaSockets.elements();
			while(clients.hasMoreElements()) {
				MinimaWebSocket client = clients.nextElement();
				if(client.getMiniDAPPUID().equals(uid)) {
					try {
						//Try and send the message..
						client.send(newmessage.toString());
						return;
					}catch(Exception exc){
						//Something wrong with this connection.. close..
						mMininaSockets.remove(client.getClientUID());
						break;
					}
				}
			}
			
			//MiniDAPP not found of that UID..
			newmessage.put("event","newreply");
			newmessage.put("error",uid+" not found");
			
			//Search again but this time back to the original
			clients = mMininaSockets.elements();
			while(clients.hasMoreElements()) {
				MinimaWebSocket client = clients.nextElement();
				if(client.getMiniDAPPUID().equals(from)) {
					try {
						//Try and send the message..
						client.send(newmessage.toString());
					}catch(Exception exc){
						//Something wrong with this connection.. close..
						mMininaSockets.remove(client.getClientUID());
					}
					
					return;
				}
			}
			
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
