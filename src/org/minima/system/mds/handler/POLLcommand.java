package org.minima.system.mds.handler;

import org.minima.system.mds.polling.PollMessage;
import org.minima.system.mds.polling.PollStack;
import org.minima.utils.json.JSONObject;

public class POLLcommand {

	PollStack mPollStack;
	
	public POLLcommand(PollStack zStack) {
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
			
			int clocksecs 	= 0;
			PollMessage msg = null;
			while(msg == null && clocksecs<60) {
				//Get the message..
				msg = mPollStack.getMessage(counter, zMiniDAPPID);
				if(msg !=null) {
					break;
				}
				
				//Wait 1 second and try again 
				try {Thread.sleep(1000);} catch (InterruptedException e) {}
				clocksecs++;
			}
			
			//Did we get a message
			if(msg != null) {
				res.put("status", true);
				res.put("response", msg.toJSON());
			}
		}
		
		//Put the latest counter
		res.put("counter", mPollStack.getCounter());
		
		//Do we have any new messages..
		String result = res.toJSONString();
		
		return result;
	}
}
