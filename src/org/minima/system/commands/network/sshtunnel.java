package org.minima.system.commands.network;

import org.minima.database.MinimaDB;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.network.minima.NIOManager;
import org.minima.system.network.p2p.P2PManager;
import org.minima.system.network.sshtunnel.SSHManager;
import org.minima.system.params.GeneralParams;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class sshtunnel extends Command {

	public sshtunnel() {
		super("sshtunnel","[enable:true|false] (user: password: host: remoteport:) - Create an SSH Tunnel for Minima to have an external IP");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		//Are we enabling..
		String enable = getParam("enable");
		if(enable.equals("")) {
			throw new Exception("MUST specify 'enable' parameter");
		}
		
		if(enable.equals("false")) {
			//Get the Object..
			MinimaDB.getDB().getUserDB().setSSHTunnelEnabled(false);
			
			//Stop SSH Tunnel.
			Main.getInstance().getNetworkManager().getSSHManager().PostMessage(SSHManager.SSHTUNNEL_STOP);
	
			//Set the GeneralParams.. too default Setting! HACK- should use command line params aswell..
			GeneralParams.IS_ACCEPTING_IN_LINKS = false;
			GeneralParams.IS_HOST_SET = false;
			GeneralParams.MINIMA_PORT = 9001;
			Main.getInstance().getNetworkManager().calculateHostIP();
			
			//Is the P2P Running
			if(GeneralParams.P2P_ENABLED) {
				//Close all current connections..
				Main.getInstance().getNIOManager().PostMessage(new Message(NIOManager.NIO_DISCONNECTALL));
				
				//Wait a few seconds..
				Thread.sleep(3000);
				
				//And you need to reset the P2P
				Main.getInstance().getNetworkManager().getP2PManager().PostMessage(P2PManager.P2P_RESET);
			}
			
			//Stopped..
			ret.put("response", "SSHTunnel stopped..");
			
			return ret;
		}
			
		//Get all the settings..
		String user 	= getParam("user");
		String password = getParam("password");
		String host 	= getParam("host");
		String rport 	= getParam("remoteport");
		
		if(user.equals("") || password.equals("") || host.equals("") || rport.equals("")) {
			throw new Exception("MUST specify all paramteres to enable ssh tunnel");
		}
		
		//Create a JSON OBject
		JSONObject ssht = new JSONObject();
		ssht.put("username", user);
		ssht.put("password", password);
		ssht.put("host", host);
		ssht.put("remoteport", rport);
		
		//And add this to the UserDB - used at startup..
		MinimaDB.getDB().getUserDB().setSSHTunnelSettings(ssht);
		
		//We are now enabled..
		MinimaDB.getDB().getUserDB().setSSHTunnelEnabled(true);
		
		//And save it..
		MinimaDB.getDB().saveUserDB();
		
		//Now send a message..
		startSSHTunnel();
		
		//Is the P2P Running
		if(GeneralParams.P2P_ENABLED) {
			//Close all current connections..
			Main.getInstance().getNIOManager().PostMessage(new Message(NIOManager.NIO_DISCONNECTALL));
			
			//Wait a few seconds..
			Thread.sleep(3000);
			
			//And you need to reset the P2P
			Main.getInstance().getNetworkManager().getP2PManager().PostMessage(P2PManager.P2P_RESET);
		}
		
		//All done..
		ret.put("response", "SSHTunnel started..");
		
		return ret;
	}

	/**
	 * If it is enabled
	 */
	public static void startSSHTunnel() {
		if(MinimaDB.getDB().getUserDB().isSSHTunnelEnabled()){
			
			//Get the Settings..
			JSONObject settings = MinimaDB.getDB().getUserDB().getSSHTunnelSettings();
			
			//Now send a message..
			Message startssh = new Message(SSHManager.SSHTUNNEL_START);
			startssh.addString("username", (String)settings.get("username"));
			startssh.addString("password", (String)settings.get("password"));
			startssh.addString("host", (String)settings.get("host"));
			startssh.addInteger("remoteport", Integer.parseInt((String)settings.get("remoteport")));
			
			//And fire away
			Main.getInstance().getNetworkManager().getSSHManager().PostMessage(startssh);
		}
	}
	
	
	@Override
	public Command getFunction() {
		return new sshtunnel();
	}

}
