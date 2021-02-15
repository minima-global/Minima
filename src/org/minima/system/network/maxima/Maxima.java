package org.minima.system.network.maxima;

import java.io.File;
import java.net.URLEncoder;
import java.nio.charset.Charset;
import java.util.ArrayList;

import org.minima.objects.base.MiniData;
import org.minima.objects.keys.MultiKey;
import org.minima.system.Main;
import org.minima.system.brains.BackupManager;
import org.minima.system.input.InputHandler;
import org.minima.system.network.maxima.db.MaximaDB;
import org.minima.system.network.maxima.db.MaximaUser;
import org.minima.utils.MinimaLogger;
import org.minima.utils.encryption.CryptoPackage;
import org.minima.utils.encryption.EncryptDecrypt;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;

public class Maxima extends MessageProcessor {

	public static final String MAXIMA_INIT      = "MAXIMA_INIT";
	public static final String MAXIMA_SHUTDOWN  = "MAXIMA_SHUTDOWN";
	
	public static final String MAXIMA_FUNCTION  = "MAXIMA_FUNCTION";
	
	public static final String MAXIMA_INFO 		= "MAXIMA_INFO";
	public static final String MAXIMA_NEW 		= "MAXIMA_NEW";
	
	public static final String MAXIMA_LISTCONTACTS  = "MAXIMA_LISTCONTACTS";
	public static final String MAXIMA_ADDCONTACT    = "MAXIMA_ADDCONTACT";
	public static final String MAXIMA_REMOVECONTACT = "MAXIMA_REMOVECONTACT";
	
	public static final String MAXIMA_RECMSG    = "MAXIMA_RECMSG";
	public static final String MAXIMA_SENDMSG   = "MAXIMA_SENDMSG";
	
	MaximaServer mServer;
	
	MaximaDB mMaximaDB;
	
	public Maxima() {
		super("MAXIMA_PROCESSOR");
			
		PostMessage(MAXIMA_INIT);
	}
	
	public void stop() {
		PostMessage(MAXIMA_SHUTDOWN);
	}
	
	public String getMaximaFullIdentity() {
		String host  = Main.getMainHandler().getNetworkHandler().getMiniMaxiHost();
		int port     = Main.getMainHandler().getNetworkHandler().getMaximaPort();
		String ident = mMaximaDB.getAccount().getPublicKey().to0xString()+"@"+host+":"+port;
		
		return ident;
	}
	
	public String getMaximaHost() {
		String host = Main.getMainHandler().getNetworkHandler().getMiniMaxiHost();
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
	
	private MaximaUser addUpdateUser(String zFrom) {
		String ident   = getIdentOnly(zFrom);
		String host    = getHostOnly(zFrom);
		long timestamp = System.currentTimeMillis();
		
		//Does that user allready exist
		MaximaUser maxuser = mMaximaDB.getUser(ident);
		if(maxuser == null) {
			//Add a new User
			maxuser = new MaximaUser(ident, host);
			mMaximaDB.addUser(maxuser);
		}else {
			//Update Host and timestamp
			maxuser.setHost(host);
		}
		
		return maxuser;
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		if(zMessage.getMessageType().equals(MAXIMA_INIT)) {
			int port = Main.getMainHandler().getNetworkHandler().getMaximaPort();
			
			//For now..
			mMaximaDB = new MaximaDB();
			
			//Get the MaximaDB
			BackupManager bk = Main.getMainHandler().getBackupManager();
			File maxim = new File(bk.getMaximaFolder(),"maxima.db");
			if(maxim.exists()) {
				mMaximaDB.loadDB(maxim);
			}else {
				//New Data..
				mMaximaDB.newAccount();
			}
			
			//Start the server
			mServer = new MaximaServer(port);
			Thread max = new Thread(mServer, "Maxima Server");
			max.setDaemon(true);
			max.start();
		
		}else if(zMessage.getMessageType().equals(MAXIMA_SHUTDOWN)) {
			if(mServer != null) {
				mServer.stop();
			}
			
			//Save the DB..
			BackupManager bk = Main.getMainHandler().getBackupManager();
			File maxim = new File(bk.getMaximaFolder(),"maxima.db");
			mMaximaDB.saveDB(maxim);
			
			//Stop this 
			stopMessageProcessor();
			
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
			InputHandler.getResponseJSON(zMessage).put("key", mMaximaDB.getAccount().toJSON());
			InputHandler.getResponseJSON(zMessage).put("identity", getMaximaFullIdentity());
//			InputHandler.getResponseJSON(zMessage).put("rsa", mMaximaDB.getPublicRSA().to0xString());
			InputHandler.endResponse(zMessage, true, "Maxima Info");
					
		}else if(zMessage.getMessageType().equals(MAXIMA_NEW)) {
			//Create a NEW key.. for now always new..
			mMaximaDB.newAccount();
			
			//Print out some info
			Message info = new Message(MAXIMA_INFO);
			InputHandler.addResponseMesage(info, zMessage);
			PostMessage(info);
			
		}else if(zMessage.getMessageType().equals(MAXIMA_ADDCONTACT)) {
			String user = zMessage.getString("maximauser");
			
			//Does that user allready exist
			MaximaUser maxuser = addUpdateUser(user);
			
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
			
			//Get the payload..
			String version   = (String) msg.get("version");
			String to        = (String) msg.get("to");
			String encrypted = (String) msg.get("encrypted");
			
			//Signature
			String sig       = (String) msg.get("signature");
			MiniData sigdata = new MiniData(sig);

			//Get the payload..
			MiniData pd = new MiniData((String)msg.get("payload"));
			
			//Is it encrypted..
			JSONObject payload = null;
			if(encrypted.equals("NONE")) {
				//Convert to a string..
				String pds = new String(pd.getData(),Charset.forName("UTF-8"));
				
				//And finally convert to a JSON
				payload = (JSONObject) new JSONParser().parse(pds);
			}else {
				//Decrypt..
				CryptoPackage cp = new CryptoPackage();
				cp.ConvertCompleteData(pd);
				
				//Now decrypt..
				byte[] decdata = cp.decrypt(mMaximaDB.getPrivateRSA().getData());
				
				//Convert to a string..
				String pds = new String(decdata,Charset.forName("UTF-8"));
				
				//And finally convert to a JSON
				payload = (JSONObject) new JSONParser().parse(pds);
			}
			
			//Who is it from..
			String from 		= (String) payload.get("from");
			String pubkey       = getIdentOnly(from);
			MiniData pubkeydata = new MiniData(pubkey);
			
			//Lets verify the signature
			MultiKey ver = new MultiKey(pubkeydata);
			boolean valid = ver.verify(pd, sigdata);

			//Is it valid..
			if(!valid) {
				MinimaLogger.log("INVALID MAXIMA : "+msg);
				InputHandler.endResponse(zMessage, false, "Invalid Message");
			}else {
				//Add the USER..
				MaximaUser maxuser = addUpdateUser(from);
				
				//Set the RSA..
				maxuser.setRSAPubKeyHex((String)payload.get("rsa"));
				
				//Sign the message Yourself
				//..
				
				MinimaLogger.log("MAXIMA "+from+" @ "+payload.get("port")+" > "+payload.get("data"));
				InputHandler.endResponse(zMessage, true, "Valid Message");
			}
			
		}else if(zMessage.getMessageType().equals(MAXIMA_SENDMSG)) {
			//Get the details..
			String to_port = zMessage.getString("to");
			String to      = new String(to_port);
			
			//Is there a Port..
			String port = "minima";
			int portind = to.indexOf(":");
			if(portind != -1) {
				to = to_port.substring(0,portind); 
				port = to_port.substring(portind+1);
			}
			
			//Get the maximauser
			MaximaUser user = mMaximaDB.getUser(to);
			if(user==null) {
				InputHandler.endResponse(zMessage, false, "User not found");
				return;
			}
			
			//Are we encrypting
			String encrypt = "NONE";
			if(!user.getRSAPubKeyHex().equals("0x00")) {
				encrypt = "RSA/AES";
			}
			
			//Get the host for this User
			String fullto = "http://"+user.getHost();
			
			//Get the message
			String message	= zMessage.getString("message");
			
			//Construct a JSON Object..
			JSONObject msg = new JSONObject();
			msg.put("version", "1.0");
			msg.put("to", user.getPublicKey());
			msg.put("encrypted", encrypt);
			
				//The content
				JSONObject data = new JSONObject();
				data.put("from", getMaximaFullIdentity());
				data.put("rsa", mMaximaDB.getPublicRSA().to0xString());
				data.put("to", to);
				data.put("port", port);
				data.put("data", message);
			
			//Copy this content..
			String content = new String(data.toString());
			JSONObject contjson = (JSONObject) new JSONParser().parse(content);
				
			//The actual data we send..
			MiniData senddata = new MiniData(data.toString().getBytes("UTF-8"));
	
			//Are we encrypting this..
			if(encrypt.equals("RSA/AES")) {
				//We encrypt..
				MiniData paydata = new MiniData(data.toString().getBytes("UTF-8"));

				//Create a crypt package
				CryptoPackage cp = new CryptoPackage();

				//Encrypt..
				cp.encrypt(paydata.getData(), new MiniData(user.getRSAPubKeyHex()).getData());
				
				//Encrypt.. with THEIR public key
				senddata =  cp.getCompleteEncryptedData();
			}
				
			//Add the data
			msg.put("payload", senddata.to0xString());
			
			//Sign the message
			MiniData sig = mMaximaDB.getAccount().sign(senddata);
			msg.put("signature", sig.to0xString());
			
			//Encode it..
			String enc = URLEncoder.encode(new String(msg.toString()),"UTF-8").trim();
			
			//Store the message
			InputHandler.getResponseJSON(zMessage).put("to", user.getCompleteAddress());
			InputHandler.getResponseJSON(zMessage).put("encryption", encrypt);
			InputHandler.getResponseJSON(zMessage).put("message", contjson);
			
			//Create a Separate Thread to send the message
			MaximaSender sender = new MaximaSender(zMessage, fullto, enc);
			Thread runner = new Thread(sender);
			runner.setDaemon(true);
			runner.start();
		}
		
	}

}
