package org.minima.system.network.sshtunnel;

import org.minima.system.input.InputHandler;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;

public class SSHTunnel extends MessageProcessor {

	public static String SSHTUNNEL_INIT   = "SSHTUNNEL_INIT";
	public static String SSHTUNNEL_INFO   = "SSHTUNNEL_INFO";
	public static String SSHTUNNEL_PARAMS = "SSHTUNNEL_PARAMS";
	public static String SSHTUNNEL_START  = "SSHTUNNEL_START";
	public static String SSHTUNNEL_STOP   = "SSHTUNNEL_STOP";

	//The SSH Tunnel manager
	sshforwarder mSSH = null;
	
	JSONObject mParams = new JSONObject();
	
	public SSHTunnel() {
		super("SSH_TUNNEL");
	
		//Initialise..
		PostMessage(SSHTUNNEL_INIT);
	}

	public void stop() {
		//Stop if Running..
		if(mSSH != null) {
			mSSH.stop();
			mSSH = null;
		}
		
		stopMessageProcessor();
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.getMessageType().equals(SSHTUNNEL_INIT)) {
			//Load the setting from the database
			
		}else if(zMessage.getMessageType().equals(SSHTUNNEL_INFO)){
			//Print the details
			InputHandler.getResponseJSON(zMessage).put("params",mParams);
			
			if(mSSH!= null) {
				InputHandler.getResponseJSON(zMessage).put("connected", mSSH.isConnected());
			}else {
				InputHandler.getResponseJSON(zMessage).put("connected", false);
			}
			
			InputHandler.endResponse(zMessage, true, "");
			
		}else if(zMessage.getMessageType().equals(SSHTUNNEL_PARAMS)){
			//Set the SSH Tunnel parameters
			String username = zMessage.getString("username");
			String password = zMessage.getString("password");
			String host     =  zMessage.getString("host");
			int remote      = zMessage.getInteger("remoteport");
					
			//do it..
			mParams.put("username", username);
			mParams.put("password", password);
			mParams.put("host", host);
			mParams.put("remoteport", ""+remote);
			
			InputHandler.getResponseJSON(zMessage).put("params",mParams);
			InputHandler.endResponse(zMessage, true, "SSH Tunnel Parameters set");
			
		}else if(zMessage.getMessageType().equals(SSHTUNNEL_START)){
			String host = (String) mParams.get("host");
			String user = (String) mParams.get("username");
			String pass = (String) mParams.get("password");
			int remotep = Integer.parseInt((String) mParams.get("remoteport"));
			
			//Start up
			mSSH = new sshforwarder(host, 22, user,pass,false, remotep);
			
			Thread tt = new Thread(mSSH);
			tt.start();
			
		}else if(zMessage.getMessageType().equals(SSHTUNNEL_STOP)){
			//Stop if Running..
			if(mSSH != null) {
				mSSH.stop();
				mSSH = null;
			}
			
			InputHandler.endResponse(zMessage,true,"SSH Tunnel stopped");		
		}
		
	}

}
