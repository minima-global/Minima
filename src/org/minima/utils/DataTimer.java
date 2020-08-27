package org.minima.utils;

import java.util.Enumeration;
import java.util.Hashtable;

public class DataTimer {

	Hashtable<String, Long> mTimedData = new Hashtable<>();
	
	public DataTimer() {}
	
	public boolean checkForData(String zData, long zMaxTime) {
		//Current time..
		long timenow     = System.currentTimeMillis();
		
		//Remove the old..
		Hashtable<String, Long> newData = new Hashtable<>();
		Enumeration<String> keys = mTimedData.keys();
		while(keys.hasMoreElements()) {
			String key = keys.nextElement();
			
			Long timeval = mTimedData.get(key);
			long time    = timeval.longValue();
			long diff    = timenow - time;
			if(diff < zMaxTime) {
				System.out.println("Data kept "+key+" "+diff);
				newData.put(key, timeval);
			}else {
				System.out.println("Data REMOVED "+key+" "+diff);
			}
		}
		
		//Swap them..
		mTimedData = newData;
		
		//Do we send this.. ?
		boolean found = mTimedData.containsKey(zData);
		if(!found) {
			System.out.println("Data addded "+zData+" "+timenow);
			mTimedData.put(zData, new Long(timenow));	
		}else {
			System.out.println("Data found! "+zData+" "+timenow);
		}
		
		return found;
	}
	
	
	public static void main(String[] zArgs) {
		
		DataTimer timer = new DataTimer();
		
		boolean found = timer.checkForData("0xFF", 1000);
		System.out.println("1 Found : "+found);
		
		found = timer.checkForData("0xFF", 1000);
		System.out.println("2 Found : "+found);
		
		try {Thread.sleep(2000);} catch (InterruptedException e) {}
		
		found = timer.checkForData("0xFF", 1000);
		System.out.println("3 Found : "+found);
		
		
	}
}
