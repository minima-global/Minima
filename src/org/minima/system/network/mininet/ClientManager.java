package org.minima.system.network.mininet;

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.Socket;
import java.util.Hashtable;

import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.system.Main;
import org.minima.utils.Streamable;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;

public class ClientManager extends MessageProcessor{

	public static ClientManager mClientManagerInstance;
	public static ClientManager getInstance() {
		return mClientManagerInstance;
	}
	
	
	public static final String CLIENT_CONNECT 			= "CLIENT_CONNECT";
	public static final String CLIENT_CONNECTATTEMPT 	= "CLIENT_CONNECTATTEMPT";
	public static final String CLIENT_NEWCONNECTION 	= "CLIENT_NEWCONNECT";
	
	public static final String CLIENT_DISCONNECT 		= "CLIENT_DISCONNECT";
	public static final String CLIENT_DISCONNECTED		= "CLIENT_DISCONNECTED";
	
	public static final String CLIENT_INCOMINGMSG 		= "CLIENT_NEWMSG";
	public static final String CLIENT_SENDMSG 			= "CLIENT_SENDMSG";
	
	Hashtable<String, ClientListener> mAllClients;
	
	public ClientManager() {
		super("MINICLIENTS");
		
		mAllClients = new Hashtable<>();
		
		mClientManagerInstance = this;
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.isMessageType(CLIENT_CONNECT)) {
			
			
		}else if(zMessage.isMessageType(CLIENT_NEWCONNECTION)) {
			
			//Get the socket..
			Socket sock = (Socket) zMessage.getObject("socket");
			
			//Incoming.. ?
			boolean incoming = zMessage.getBoolean("incoming");
			
			//Set up a Reader..
			ClientListener client = new ClientListener(sock, incoming);
			
			//Add to our list
			mAllClients.put(client.getUID(), client);
			
			//Now start a thread to run it..
			Thread reader = new Thread(client);
			reader.start();
			
		}else if(zMessage.isMessageType(CLIENT_DISCONNECT)) {
			
		
		}else if(zMessage.isMessageType(CLIENT_INCOMINGMSG)) {
			
			//Who from
			String uid = zMessage.getString("uid");
			
			//What's the message
			MiniData data = (MiniData) zMessage.getObject("data");
			
			//Process that message
			ClientMessage newmess = new ClientMessage(uid, data);
			
			//Process in thread pool!..
			newmess.run();
			
		}else if(zMessage.isMessageType(CLIENT_SENDMSG)) {
			
			//Who to..
			String uid = zMessage.getString("uid");
			
			//Send a Message..
			if(uid.equals("")) {
				//Send to all..
				
			}else {
				
				//get the client..
				
				
			}
			
		}
		
	}

	
	/**
	 * Small delay before actually posting the request.. 2 second..
	 */
	public static void sendDelayedTxPoWReq(String zClientID, String zTxPoWID, String zReason) {
//		TimerMessage timed = new TimerMessage(2000, NIO_TXPOWREQ);
//		timed.addString("client", zClientID);
//		timed.addString("txpowid", zTxPoWID);
//		timed.addString("reason",zReason);
		
//		MinimaLogger.log("DELAYED Request : "+zTxPoWID+" "+zReason);
		
//		Main.getInstance().getNIOManager().PostTimerMessage(timed);
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
		
		//For ALL or for ONE
		if(!zUID.equals("")) {
			//Send it..
			Main.getInstance().getNIOManager().getNIOServer().sendMessage(zUID,data);
		}else {
			//Send it..
			Main.getInstance().getNIOManager().getNIOServer().sendMessageAll(data);
		}
	}
}
