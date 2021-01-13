package org.minima.system.network.maxima;

import java.io.IOException;
import java.net.URLEncoder;
import java.nio.charset.Charset;

import org.minima.objects.base.MiniData;
import org.minima.objects.keys.MultiKey;
import org.minima.system.Main;
import org.minima.system.input.InputHandler;
import org.minima.system.network.rpc.RPCClient;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;

public class Maxima extends MessageProcessor {

	public static final String MAXIMA_INIT      = "MAXIMA_INIT";
	
	public static final String MAXIMA_FUNCTION  = "MAXIMA_FUNCTION";
	
	public static final String MAXIMA_INFO 		= "MAXIMA_INFO";
	
	public static final String MAXIMA_NEW 		= "MAXIMA_NEW";
	
	public static final String MAXIMA_RECMSG    = "MAXIMA_RECMSG";
	public static final String MAXIMA_SENDMSG   = "MAXIMA_SENDMSG";
	
	MaximaServer mServer;
	
	MultiKey mIdentity;
	
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
	
	public String getMaximaIdentity() {
		String host = Main.getMainHandler().getNetworkHandler().getBaseHost();
		int port    = Main.getMainHandler().getNetworkHandler().getMaximaPort();
		String ident = mIdentity.getPublicKey().to0xString()+"@"+host+":"+port;
		
		return ident;
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		if(zMessage.getMessageType().equals(MAXIMA_INIT)) {
			int port = Main.getMainHandler().getNetworkHandler().getMaximaPort();
			
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
			
			}else if(func.equals("info")) {
				Message info = new Message(MAXIMA_INFO);
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
			InputHandler.getResponseJSON(zMessage).put("identity", getMaximaIdentity());
			InputHandler.endResponse(zMessage, true, "Maxima Info");
					
		}else if(zMessage.getMessageType().equals(MAXIMA_NEW)) {
			//Create a NEW key.. for now always new..
			mIdentity = new MultiKey(160);
			
			Message info = new Message(MAXIMA_INFO);
			InputHandler.addResponseMesage(info, zMessage);
			PostMessage(info);
			
		}else if(zMessage.getMessageType().equals(MAXIMA_RECMSG)) {
			String datastr	= zMessage.getString("message");
			MiniData data   = new MiniData(datastr);
			String json     = new String(data.getData(), Charset.forName("UTF-8"));
			
			//Convert Message to JSON
			JSONObject msg = (JSONObject) new JSONParser().parse(json);

			//Received a message.. check the signature..
			MinimaLogger.log("MAXIMA REC : "+msg.toString());
			
			InputHandler.endResponse(zMessage, true, "Valid Message");
		
		}else if(zMessage.getMessageType().equals(MAXIMA_SENDMSG)) {
			//Get the details..
			String to 		= zMessage.getString("to");
			if(!to.startsWith("http")) {
				to = "http://"+to;
			}
			
			String message	= zMessage.getString("message");
			
			//Sign the Hash using your Maxima Key
			byte[] msgdata   = message.getBytes("UTF-8");
			MiniData hash    = new MiniData( Crypto.getInstance().hashData(msgdata,160) );
			
			//For now  random.. 
			MiniData sig     = mIdentity.sign(hash);
			
			//Construct a JSON Object..
			JSONObject msg = new JSONObject();
			
			msg.put("from", getMaximaIdentity());
			msg.put("to", to);
			msg.put("data", message);
			
			msg.put("hash", hash.to0xString());
			msg.put("signature", sig.to0xString());
			
			//Encode it..
			String enc = URLEncoder.encode(new String(msg.toString()),"UTF-8").trim();
			
			//Store the message
			InputHandler.getResponseJSON(zMessage).put("message", msg);
			
			//Create a Separate Thread to send the message
			MaximaSender sender = new MaximaSender(zMessage, to, enc);
			Thread runner = new Thread(sender);
			runner.setDaemon(true);
			runner.start();
		}
		
	}

}
