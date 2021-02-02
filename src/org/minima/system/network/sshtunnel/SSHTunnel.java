package org.minima.system.network.sshtunnel;

import java.io.File;

import org.minima.system.Main;
import org.minima.system.brains.BackupManager;
import org.minima.system.input.InputHandler;
import org.minima.utils.JsonDB;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;

import com.jcraft.jsch.JSch;
import com.jcraft.jsch.Logger;

public class SSHTunnel extends MessageProcessor {

	public static String SSHTUNNEL_INIT     = "SSHTUNNEL_INIT";
	public static String SSHTUNNEL_SHUTDOWN = "SSHTUNNEL_SHUTDOWN";
	
	public static String SSHTUNNEL_INFO    = "SSHTUNNEL_INFO";
	public static String SSHTUNNEL_LOGGING = "SSHTUNNEL_LOGGING";
	public static String SSHTUNNEL_PARAMS  = "SSHTUNNEL_PARAMS";
	public static String SSHTUNNEL_CLEAR   = "SSHTUNNEL_CLEAR";
	
	public static String SSHTUNNEL_START   = "SSHTUNNEL_START";
	public static String SSHTUNNEL_STOP    = "SSHTUNNEL_STOP";

	//The SSH Tunnel manager
	sshforwarder mSSH = null;
	
	JsonDB mTunnelDB;
	
	boolean mLogging = false;
	
	public SSHTunnel() {
		super("SSH_TUNNEL");
	
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
					MinimaLogger.log("SSH TUNNEL : "+zLevel+") "+zLog);
				}
				
				@Override
				public boolean isEnabled(int zLevel) {
					return mLogging;
				}
			});
			
			//New DB
			mTunnelDB = new JsonDB();
			
			//Load the setting from the database
			BackupManager bk = Main.getMainHandler().getBackupManager();
			File tunnel = new File(bk.getSSHTunnelFolder(),"tunnel.db");
			if(tunnel.exists()) {
				mTunnelDB.loadDB(tunnel);
			}
			
			//Does it exist..
			if(mTunnelDB.exists("host")) {
				//Boot her up!
				PostMessage(SSHTUNNEL_START);
			}
		
		}else if(zMessage.getMessageType().equals(SSHTUNNEL_SHUTDOWN)){
			//Stop if Running..
			if(mSSH != null) {
				mSSH.stop();
				mSSH = null;
			}
			
			//Save the DB
			BackupManager bk = Main.getMainHandler().getBackupManager();
			File tunnel = new File(bk.getSSHTunnelFolder(),"tunnel.db");
			mTunnelDB.saveDB(tunnel);
			
			//Stop this..
			stopMessageProcessor();
		
		}else if(zMessage.getMessageType().equals(SSHTUNNEL_CLEAR)){
			mTunnelDB.clean();
			InputHandler.endResponse(zMessage, true, "SSH params cleared");

		}else if(zMessage.getMessageType().equals(SSHTUNNEL_LOGGING)){
			mLogging = zMessage.getBoolean("logging");
			
			if(mLogging) {
				InputHandler.endResponse(zMessage, true, "SSH Logging ENABLED");
			}else {
				InputHandler.endResponse(zMessage, true, "SSH Logging DISABLED");
			}
			
		}else if(zMessage.getMessageType().equals(SSHTUNNEL_INFO)){
			//Print the details
			InputHandler.getResponseJSON(zMessage).put("params",mTunnelDB.getAllData());
			InputHandler.getResponseJSON(zMessage).put("logging",mLogging);
			
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
			String host     = zMessage.getString("host");
			String remote   = zMessage.getString("remoteport");
		
			//Get the parameters
			JSONObject params = mTunnelDB.getAllData();
			
			//do it..
			params.put("username", username);
			params.put("password", password);
			params.put("host", host);
			params.put("remoteport", remote);
			
			InputHandler.getResponseJSON(zMessage).put("params",params);
			InputHandler.endResponse(zMessage, true, "SSH Tunnel Parameters set");
			
		}else if(zMessage.getMessageType().equals(SSHTUNNEL_START)){
			//Get the parameters
			JSONObject params = mTunnelDB.getAllData();
			
			String host = (String) params.get("host");
			String user = (String) params.get("username");
			String pass = (String) params.get("password");
			int remotep = Integer.parseInt((String) params.get("remoteport"));
			
			//Start up
			mSSH = new sshforwarder(host, 22, user,pass,false, (int)remotep);
			Thread tt = new Thread(mSSH);
			tt.start();
			
			InputHandler.endResponse(zMessage, true, "SSH tunnel started");
			
		}else if(zMessage.getMessageType().equals(SSHTUNNEL_STOP)){
			//Stop if Running..
			if(mSSH != null) {
				mSSH.stop();
				mSSH = null;
			}
			
			InputHandler.endResponse(zMessage,true,"SSH Tunnel stopped");		
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
