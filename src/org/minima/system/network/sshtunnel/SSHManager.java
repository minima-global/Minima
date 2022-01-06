package org.minima.system.network.sshtunnel;

import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;

import com.jcraft.jsch.JSch;
import com.jcraft.jsch.Logger;

public class SSHManager extends MessageProcessor {

	public static String SSHTUNNEL_INIT     = "SSHTUNNEL_INIT";
	public static String SSHTUNNEL_SHUTDOWN = "SSHTUNNEL_SHUTDOWN";
	
	public static String SSHTUNNEL_INFO    = "SSHTUNNEL_INFO";
	public static String SSHTUNNEL_LOGGING = "SSHTUNNEL_LOGGING";
	public static String SSHTUNNEL_PARAMS  = "SSHTUNNEL_PARAMS";
	public static String SSHTUNNEL_CLEAR   = "SSHTUNNEL_CLEAR";
	
	public static String SSHTUNNEL_START   = "SSHTUNNEL_START";
	public static String SSHTUNNEL_STOP    = "SSHTUNNEL_STOP";

	//The SSH Tunnel manager
	SSHForwarder mSSH = null;
	
	boolean mLogging = true;
	
	public SSHManager() {
		super("SSH_MANAGER");
	
		//Initialise..
		PostMessage(SSHTUNNEL_INIT);
	}

	public void stop() {
		PostMessage(SSHTUNNEL_SHUTDOWN);
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.getMessageType().equals(SSHTUNNEL_INIT)) {
			//Some logging..
			JSch.setLogger(new Logger() {
				@Override
				public void log(int zLevel, String zLog) {
					MinimaLogger.log("[SSH_TUNNEL] "+zLevel+") "+zLog);
				}
				
				@Override
				public boolean isEnabled(int zLevel) {
					return mLogging;
				}
			});
			
		
		}else if(zMessage.getMessageType().equals(SSHTUNNEL_SHUTDOWN)){
			//Stop if Running..
			stopSSHTunnel();
			
			//Stop this..
			stopMessageProcessor();
		
		}else if(zMessage.getMessageType().equals(SSHTUNNEL_CLEAR)){
//			mTunnelDB.clean();
//			InputHandler.endResponse(zMessage, true, "SSH params cleared");

		}else if(zMessage.getMessageType().equals(SSHTUNNEL_LOGGING)){
			mLogging = zMessage.getBoolean("logging");
			
		}else if(zMessage.getMessageType().equals(SSHTUNNEL_INFO)){
//			//Print the details
//			InputHandler.getResponseJSON(zMessage).put("params",mTunnelDB.getAllData());
//			InputHandler.getResponseJSON(zMessage).put("logging",mLogging);
//			
//			if(mSSH!= null) {
//				InputHandler.getResponseJSON(zMessage).put("connected", mSSH.isConnected());
//			}else {
//				InputHandler.getResponseJSON(zMessage).put("connected", false);
//			}
//			
//			InputHandler.endResponse(zMessage, true, "");
			
		}else if(zMessage.getMessageType().equals(SSHTUNNEL_PARAMS)){
//			//Set the SSH Tunnel parameters
//			String username = zMessage.getString("username");
//			String password = zMessage.getString("password");
//			String host     = zMessage.getString("host");
//			String remote   = zMessage.getString("remoteport");
//		
//			//Get the parameters
//			JSONObject params = mTunnelDB.getAllData();
//			
//			//do it..
//			params.put("username", username);
//			params.put("password", password);
//			params.put("host", host);
//			params.put("remoteport", remote);
//			
//			InputHandler.getResponseJSON(zMessage).put("params",params);
//			InputHandler.endResponse(zMessage, true, "SSH Tunnel Parameters set");
			
		}else if(zMessage.getMessageType().equals(SSHTUNNEL_START)){
			
			//Are we already running..
			stopSSHTunnel();
			
			String host = zMessage.getString("host");
			String user = zMessage.getString("username");
			String pass = zMessage.getString("password");
			int remotep = zMessage.getInteger("remoteport");
			
			//Start up
			mSSH = new SSHForwarder(host, 22, user, pass, false, remotep);
			Thread tt = new Thread(mSSH);
			tt.start();
			
		}else if(zMessage.getMessageType().equals(SSHTUNNEL_STOP)){
			//Stop if Running..
			stopSSHTunnel();
		}
		
	}
	
	private void stopSSHTunnel() {
		if(mSSH != null) {
			mSSH.stop();
			mSSH = null;
			
			//Wait for it..
			try {Thread.sleep(5000);} catch (InterruptedException e) {}
		}
	}

	public static void main(String[] zArgs) {
		JSONObject json = new JSONObject();
		
		json.put("hello", 1);
		json.put("bool", true);
		
		System.out.println(json);
		
		int t = (int) json.get("hello");
		boolean bb = (boolean) json.get("bool");
		
		System.out.println(t+" "+bb);
		
	}
	
}
