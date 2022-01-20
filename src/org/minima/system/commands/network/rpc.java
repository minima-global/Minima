package org.minima.system.commands.network;

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
