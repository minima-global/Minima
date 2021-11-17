package org.minima.system.commands.all;

import org.minima.database.MinimaDB;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONObject;

public class incentivecash extends Command {

	public incentivecash() {
		super("incentivecash","(uid:) - Show or Specify your UserID for the Incentive Cash program");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		if(existsParam("uid")) {
			String uid = getParam("uid");
			
			//Set this in the UserDB
			MinimaDB.getDB().getUserDB().setIncentiveCashUserID(uid);
			
			//Post a message
			Main.getInstance().PostMessage(Main.MAIN_INCENTIVE);
		}
		
		//Show the details..
		ret.put("response", MinimaDB.getDB().getUserDB().getIncentiveCashUserID());
		
		MinimaDB.getDB().saveUserDB();
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new incentivecash();
	}

}
