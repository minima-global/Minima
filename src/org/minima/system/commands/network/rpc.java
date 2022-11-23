package org.minima.system.commands.network;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.params.GeneralParams;
import org.minima.utils.json.JSONObject;

public class rpc extends Command {

	public rpc() {
		super("rpc","[enable:true|false] - Enable and disable RPC on port "+GeneralParams.RPC_PORT+" (default is off)");
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
				+ "Examples:\n"
				+ "\n"
				+ "rpc enable:true\n"
				+ "\n"
				+ "rpc enable:false\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"enable"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		//Enable or Disable
		String enable = getParam("enable");
		
		if(enable.equals("true")) {
			MinimaDB.getDB().getUserDB().setRPCEnabled(true);
		}else {
			MinimaDB.getDB().getUserDB().setRPCEnabled(false);
		}
		
		//Save the state..
		MinimaDB.getDB().saveUserDB();
		
		//Get the Network manager on it..
		boolean enabled = MinimaDB.getDB().getUserDB().isRPCEnabled();
		
		if(enabled) {
			Main.getInstance().getNetworkManager().startRPC();
		}else {
			Main.getInstance().getNetworkManager().stopRPC();
		}
		
		ret.put("response", "RPC enable : "+enabled);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new rpc();
	}

}
