package org.minima.utils;

import java.util.Enumeration;
import java.util.Hashtable;

public class DataTimer {

	Hashtable<String, Long> mTimedData = new Hashtable<>();
	
	public DataTimer() {}
	
	public boolean checkForData(String zData, long zMaxTime) {
		//Current time..
		long timenow     = System.currentTimeMillis();
		int tot = 0;
		
		//Remove the old..
		Hashtable<String, Long> newData = new Hashtable<>();
		Enumeration<String> keys = mTimedData.keys();
		while(keys.hasMoreElements()) {
			String key = keys.nextElement();
			
			//Remove after 10 minutes
			Long timeval = mTimedData.get(key);
			long time    = timeval.longValue();
			long diff    = timenow - time;
			if(diff < zMaxTime) {
				newData.put(key, timeval);
			}else {
				//It's being removed..
				tot++;
			}
		}
		
		//Swap them..
		mTimedData = newData;
		
		//Do we send this.. ?
		boolean sender = (mTimedData.get(zData) == null);
		if(sender) {
			mTimedData.put(zData, new Long(timenow));	
		}
		
		return sender;
	}
}
