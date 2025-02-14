package org.minima.system.mds.handler;

import java.util.ArrayList;

import org.minima.system.mds.polling.PollMessage;
import org.minima.system.mds.polling.PollStack;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class MEGAPOLLcommand {

	PollStack mPollStack;
	
	public MEGAPOLLcommand(PollStack zStack) {
		mPollStack = zStack;
	}
	
	public String runCommand(String zMiniDAPPID, String zData) {
		
		//Get the series and counter
		int index 			= zData.indexOf("&");
		String fullseries 	= zData.substring(0,index);
		index 				= fullseries.indexOf("=");
		String series		= fullseries.substring(index+1);
		String fullcounter 	= zData.substring(index+1);
		index 				= fullcounter.indexOf("=");
		String strcounter	= fullcounter.substring(index+1);
		int counter 		= Integer.parseInt(strcounter);
		
		//The returned data..
		JSONObject res = new JSONObject();
		res.put("series", mPollStack.getSeries());
		res.put("counter", mPollStack.getCounter());
		res.put("status", false);
		
		//Are we on the correct series.. 
		if(mPollStack.getSeries().equals(series)) {
			
			//We are on the correct series
			res.put("status", true);
			
			//Get all the latest message
			ArrayList<PollMessage> msgs = mPollStack.getAllMessages(counter, zMiniDAPPID);
			
			//Convert to a JSON array
			JSONArray pollarr = new JSONArray();
			for(PollMessage pmsg : msgs) {
				pollarr.add(pmsg.toJSON());
			}
			
			//Add them
			res.put("response", pollarr);
		}
		
		//Do we have any new messages..
		String result = res.toJSONString();
		
		return result;
	}
}
