package org.minima.system.commands.network;

import java.security.cert.Certificate;
import java.util.ArrayList;
import java.util.Arrays;

import org.minima.objects.base.MiniData;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.params.GeneralParams;
import org.minima.utils.json.JSONObject;
import org.minima.utils.ssl.SSLManager;

public class rpc extends Command {

	public rpc() {
		super("rpc","(enable:) (ssl:) (password:) - Enable and disable RPC on port "+GeneralParams.RPC_PORT+" (default is off)");
	}
	
	@Override
	public String getFullHelp() {
		return "\nrpc\n"
				+ "\n"
				+ "Enable and disable RPC on port "+GeneralParams.RPC_PORT+" (default is off).\n"
				+ "\n"
				+ "Ensure your RPC port is secured behind a firewall before enabling.\n"
				+ "\n"
				+ "enable:\n"
				+ "    true or false, true to enable rpc or false to disable.\n"
				+ "\n"
				+ "ssl:\n"
				+ "    true or false, true to enable Self signed SSL - you can use stunnel yourself.\n"
				+ "\n"
				+ "password:\n"
				+ "    the Basic Auth password used in headers - ONLY secure if used with SSL.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "rpc enable:true\n"
				+ "\n"
				+ "rpc enable:true ssl:true password:minima\n"
				+ "\n"
				+ "rpc enable:true password:minima\n"
				+ "\n"
				+ "rpc enable:false\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"enable","ssl","password"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		if(existsParam("enable")) {
		
			//Enable or Disable
			boolean enable = getBooleanParam("enable");
			
			//Stop the Old
			Main.getInstance().getNetworkManager().stopRPC();
			Thread.sleep(1000);
		
			//Store
			GeneralParams.RPC_ENABLED = enable;
			
			//Are we setting a password or SSL
			GeneralParams.RPC_SSL = getBooleanParam("ssl", false);
	
			//Is there a password
			if(existsParam("password")) {
				GeneralParams.RPC_PASSWORD = getParam("password");
				GeneralParams.RPC_AUTHENTICATE = true;
			}else {
				GeneralParams.RPC_AUTHENTICATE = false;
			}
			
			//Now start or stop..
			if(enable) {
				Main.getInstance().getNetworkManager().startRPC();
			}else {
				Main.getInstance().getNetworkManager().stopRPC();
			}
		}
		
		
		JSONObject rpcdets = new JSONObject();
		rpcdets.put("enabled", GeneralParams.RPC_ENABLED);
		rpcdets.put("port", GeneralParams.RPC_PORT);
		rpcdets.put("ssl", GeneralParams.RPC_SSL);
		
		//Get the Public Key..
		Certificate cert 	= SSLManager.getSSLKeyStore().getCertificate("MINIMA_NODE");
		MiniData pubk 		= new MiniData(cert.getPublicKey().getEncoded());
		rpcdets.put("sslpubkey",pubk.to0xString());
		
		rpcdets.put("authenticate", GeneralParams.RPC_AUTHENTICATE);
		rpcdets.put("password", "***");
		
		ret.put("response", rpcdets);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new rpc();
	}

}
