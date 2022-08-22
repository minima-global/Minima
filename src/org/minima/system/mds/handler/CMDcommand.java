package org.minima.system.mds.handler;

import org.minima.system.commands.Command;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class CMDcommand {

	String mMiniDAPPID;
	String mCompleteCommand;
	
	public CMDcommand(String zMiniDAPPID, String zCommand) {
		mMiniDAPPID 		= zMiniDAPPID;
		mCompleteCommand 	= zCommand;
	}
	
	public String runCommand() {
		
		//Default fail result
		JSONObject statfalse = new JSONObject();
		statfalse.put("command", mCompleteCommand);
		statfalse.put("status", false);
		statfalse.put("pending", false);
		String result = statfalse.toJSONString();
		
		try {
			//Now run this function..
			JSONArray res = Command.runMultiCommand(mMiniDAPPID,mCompleteCommand);
			
			//Get the result.. is it a multi command or single.. 
			if(res.size() == 1) {
				result = res.get(0).toString();
			}else {
				result = res.toString();
			}
			
		}catch(Exception exc) {
			MinimaLogger.log("ERROR CMDHANDLER : "+mCompleteCommand+" "+exc);
			
			//Add the error
			statfalse.put("error", exc.toString());
			result = statfalse.toJSONString();
		}
		
		return result;
	}
	

}
