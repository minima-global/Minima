package org.minima.system.commands.all;

import org.minima.database.MinimaDB;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.network.sshtunnel.SSHManager;
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
	
			//Stopped..
			ret.put("response", "SSHTunnel stopped..");
			
			return ret;
		}
		
		if(!enable.equals("true")) {
			throw new Exception("Incorrect value for enable param : "+enable);
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
