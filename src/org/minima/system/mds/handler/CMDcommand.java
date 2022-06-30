package org.minima.system.mds.handler;

import org.minima.system.commands.Command;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class CMDcommand {

	public CMDcommand() {}
	
	public String runCommand(String zMiniDAPPID, String zCMD) {
		
		JSONObject statfalse = new JSONObject();
		statfalse.put("status", false);
		String result = statfalse.toJSONString();
		
		try {
			//Now run this function..
			JSONArray res = Command.runMultiCommand(zCMD);
	    	
			//Get the result.. is it a multi command or single.. 
			if(res.size() == 1) {
				result = res.get(0).toString();
			}else {
				result = res.toString();
			}
			
		}catch(Exception exc) {
			MinimaLogger.log("ERROR CMDHANDLER : "+zCMD+" "+exc);
		}
		
		return result;
	}
	

}
