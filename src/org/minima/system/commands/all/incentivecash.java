package org.minima.system.commands.all;

import org.minima.database.MinimaDB;
import org.minima.system.commands.Command;
import org.minima.utils.RPCClient;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;

public class incentivecash extends Command {

	public incentivecash() {
		super("incentivecash","(uid:) - Show your rewards or specify your UserID for the Incentive Cash program");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		if(existsParam("uid")) {
			String uid = getParam("uid");
			
			//Set this in the UserDB
			MinimaDB.getDB().getUserDB().setIncentiveCashUserID(uid);
			
			//Save this..
			MinimaDB.getDB().saveUserDB();
		}

		//Get the User
		String user = MinimaDB.getDB().getUserDB().getIncentiveCashUserID();
		
		//Show the details..
		JSONObject ic = new JSONObject();
		ic.put("uid", user);
				
		//Make sure there is a User specified
		if(!user.equals("")) {
			//Call the RPC End point..
			String reply = RPCClient.sendPUT("https://incentivecash.minima.global/api/ping/"+user);
			
			//Convert response..
			JSONObject json = (JSONObject) new JSONParser().parse(reply);
			
			ic.put("details", json);
		}
		
		ret.put("response", ic);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new incentivecash();
	}

}
