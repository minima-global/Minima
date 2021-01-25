package org.minima.system.network.maxima;

import java.io.IOException;
import java.net.URLEncoder;
import java.nio.charset.Charset;
import java.util.ArrayList;

import org.minima.objects.base.MiniData;
import org.minima.objects.keys.MultiKey;
import org.minima.system.Main;
import org.minima.system.input.InputHandler;
import org.minima.system.network.maxima.db.MaximaDB;
import org.minima.system.network.maxima.db.MaximaUser;
import org.minima.system.network.rpc.RPCClient;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;

public class Maxima extends MessageProcessor {

	public static final String MAXIMA_INIT      = "MAXIMA_INIT";
	
	public static final String MAXIMA_FUNCTION  = "MAXIMA_FUNCTION";
	
	public static final String MAXIMA_INFO 		= "MAXIMA_INFO";
	public static final String MAXIMA_NEW 		= "MAXIMA_NEW";
	
	public static final String MAXIMA_LISTCONTACTS  = "MAXIMA_LISTCONTACTS";
	public static final String MAXIMA_ADDCONTACT    = "MAXIMA_ADDCONTACT";
	public static final String MAXIMA_REMOVECONTACT = "MAXIMA_REMOVECONTACT";
	
	public static final String MAXIMA_RECMSG    = "MAXIMA_RECMSG";
	public static final String MAXIMA_SENDMSG   = "MAXIMA_SENDMSG";
	
	MaximaServer mServer;
	
	MultiKey mIdentity;
	
	MaximaDB mMaximaDB;
	
	public Maxima() {
		super("MAXIMA_PROCESSOR");
			
		PostMessage(MAXIMA_INIT);
	}
	
	public void stop() {
		if(mServer != null) {
			mServer.stop();
		}
		
		stopMessageProcessor();
	}
	
	public String getMaximaFullIdentity() {
		String host = Main.getMainHandler().getNetworkHandler().getBaseHost();
		int port    = Main.getMainHandler().getNetworkHandler().getMaximaPort();
		String ident = mIdentity.getPublicKey().to0xString()+"@"+host+":"+port;
		
		return ident;
	}
	
	public String getMaximaHost() {
		String host = Main.getMainHandler().getNetworkHandler().getBaseHost();
		int port    = Main.getMainHandler().getNetworkHandler().getMaximaPort();
		return host+":"+port;
	}
	
	public String getIdentOnly(String zFullIdentity) {
		int index = zFullIdentity.indexOf("@");
		return zFullIdentity.substring(0, index);
	}
	
	public String getHostOnly(String zFullIdentity) {
		int index = zFullIdentity.indexOf("@");
		return zFullIdentity.substring(index+1);
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		if(zMessage.getMessageType().equals(MAXIMA_INIT)) {
			int port = Main.getMainHandler().getNetworkHandler().getMaximaPort();
			
			//For now..
			mMaximaDB = new MaximaDB();
			
			//Create a NEW key.. for now always new..
			mIdentity = new MultiKey(160);
			
			//Start the server
			mServer = new MaximaServer(port);
			Thread max = new Thread(mServer, "Maxima Server");
			max.setDaemon(true);
			max.start();
		
		}else if(zMessage.getMessageType().equals(MAXIMA_FUNCTION)) {
			String func = zMessage.getString("function");
			
			if(func.equals("send")) {
				Message sender = new Message(MAXIMA_SENDMSG);
				sender.addString("to", zMessage.getString("to"));
				sender.addString("message", zMessage.getString("message"));
				InputHandler.addResponseMesage(sender, zMessage);
			
				PostMessage(sender);
				return;
			
			}else if(func.equals("receive")) {
				Message sender = new Message(MAXIMA_RECMSG);
				sender.addString("message", zMessage.getString("message"));
				InputHandler.addResponseMesage(sender, zMessage);
			
				PostMessage(sender);
				return;
			
			}else if(func.equals("add")) {
				Message sender = new Message(MAXIMA_ADDCONTACT);
				sender.addString("maximauser", zMessage.getString("maximauser"));
				InputHandler.addResponseMesage(sender, zMessage);
				PostMessage(sender);
				return;
			
			}else if(func.equals("info")) {
				Message info = new Message(MAXIMA_INFO);
				InputHandler.addResponseMesage(info, zMessage);
				PostMessage(info);
				return;
			
			}else if(func.equals("list")) {
				Message info = new Message(MAXIMA_LISTCONTACTS);
				InputHandler.addResponseMesage(info, zMessage);
				PostMessage(info);
				return;
			
			}else if(func.equals("new")) {
				Message info = new Message(MAXIMA_NEW);
				InputHandler.addResponseMesage(info, zMessage);
				PostMessage(info);
				return;
			}
				
			InputHandler.endResponse(zMessage, false, "Invalid maxima function : "+func);
		
		}else if(zMessage.getMessageType().equals(MAXIMA_INFO)) {
			InputHandler.getResponseJSON(zMessage).put("key", mIdentity.toJSON());
			InputHandler.getResponseJSON(zMessage).put("identity", getMaximaFullIdentity());
			InputHandler.endResponse(zMessage, true, "Maxima Info");
					
		}else if(zMessage.getMessageType().equals(MAXIMA_NEW)) {
			//Create a NEW key.. for now always new..
			mIdentity = new MultiKey(160);
			
			Message info = new Message(MAXIMA_INFO);
			InputHandler.addResponseMesage(info, zMessage);
			PostMessage(info);
			
		}else if(zMessage.getMessageType().equals(MAXIMA_ADDCONTACT)) {
			String user = zMessage.getString("maximauser");
			
			String ident   = getIdentOnly(user);
			String host    = getHostOnly(user);
			long timestamp = System.currentTimeMillis();
			
			//Does that user allready exist
			MaximaUser maxuser = mMaximaDB.getUser(ident);
			if(maxuser == null) {
				//Add a new User
				maxuser = new MaximaUser(ident, host, timestamp);
				mMaximaDB.addUser(maxuser);
			}else {
				//Update Host and timestamp
				maxuser.setHost(host);
			}
			
			InputHandler.getResponseJSON(zMessage).put("user", maxuser.toJSON());
			InputHandler.endResponse(zMessage, true, "User added");
			
		}else if(zMessage.getMessageType().equals(MAXIMA_REMOVECONTACT)) {
		
		}else if(zMessage.getMessageType().equals(MAXIMA_LISTCONTACTS)) {
			JSONObject resp = InputHandler.getResponseJSON(zMessage);
		
			JSONArray allusers = new JSONArray();
			ArrayList<MaximaUser> users = mMaximaDB.getAllUsers();
			for(MaximaUser mx : users) {
				allusers.add(mx.toJSON());
			}
			
			resp.put("total", allusers.size());
			resp.put("contacts", allusers);
			InputHandler.endResponse(zMessage, true, "");
		
		}else if(zMessage.getMessageType().equals(MAXIMA_RECMSG)) {
			String datastr	= zMessage.getString("message");
			MiniData data   = new MiniData(datastr);
			String json     = new String(data.getData(), Charset.forName("UTF-8"));
			
			//Convert Message to JSON
			JSONObject msg = (JSONObject) new JSONParser().parse(json);
			MinimaLogger.log("MAXIMA REC : "+msg);
			
			//Get the payload..
			JSONObject payload = (JSONObject) msg.get("payload");
			MinimaLogger.log("PAYLOAD : "+payload.toString());
			
			
			
			InputHandler.endResponse(zMessage, true, "Valid Message");
			
//			//Who sent it..
//			String sender = (String) msg.get("from");
//			MiniData pubkey = new MiniData(sender);
//			
//			//Check the Signature
//			String sentmsg = (String) msg.get("data");
//			MiniData msgdata    = new MiniData( sentmsg.getBytes("UTF-8") );
//			
//			String sig      = (String) msg.get("signature");
//			MiniData sigdat = new MiniData(sig);
//			
//			MultiKey ver = new MultiKey(pubkey);
//			boolean valid = ver.verify(msgdata, sigdat);
//			if(!valid) {
//				MinimaLogger.log("INVALID MAXIMA : "+msg);
//				InputHandler.endResponse(zMessage, false, "Invalid Message");
//			}else {
//				MinimaLogger.log("MAXIMA "+msg.get("from")+" > "+msg.get("data"));
//				InputHandler.endResponse(zMessage, true, "Valid Message");
//			}
			
		}else if(zMessage.getMessageType().equals(MAXIMA_SENDMSG)) {
			//Get the details..
			String to = zMessage.getString("to");
			
			//Get the maximauser
			MaximaUser user = mMaximaDB.getUser(to);
			
			if(user==null) {
				InputHandler.endResponse(zMessage, false, "User not found");
				return;
			}
			
			//Get the host for this User
			String fullto = "http://"+user.getHost();
			
			//Get the message
			String message	= zMessage.getString("message");
			
			//Construct a JSON Object..
			JSONObject msg = new JSONObject();
			msg.put("version", 1);
			msg.put("from", mIdentity.getPublicKey().to0xString());
			msg.put("host", getMaximaHost());
			msg.put("to", user.getCompleteAddress());
			
				//The content
				JSONObject data = new JSONObject();
				data.put("to", to);
				data.put("port", "minima");
				data.put("data", message);
			
			//Add the data
			msg.put("payload", data);
				
			//The Signature of the Data
//			MiniData senddata = new MiniData(message.getBytes("UTF-8"));
//			
//			//Sign the message
//			MiniData sig     = mIdentity.sign(senddata);
			//msg.put("signature", sig.to0xString());
			
			//Encode it..
			String enc = URLEncoder.encode(new String(msg.toString()),"UTF-8").trim();
			
			//Store the message
			InputHandler.getResponseJSON(zMessage).put("message", msg);
			
			//Create a Separate Thread to send the message
			MaximaSender sender = new MaximaSender(zMessage, fullto, enc);
			Thread runner = new Thread(sender);
			runner.setDaemon(true);
			runner.start();
		}
		
	}

}
