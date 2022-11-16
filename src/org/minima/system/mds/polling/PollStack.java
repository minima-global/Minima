package org.minima.system.mds.polling;

import java.util.ArrayList;

import org.minima.objects.base.MiniData;
import org.minima.utils.json.JSONObject;

public class PollStack {

	public static final int MAX_MESSAGES = 256;
	
	String mSeries;
	
	int mCounter;
	
	ArrayList<PollMessage> mMessages;
	
	public PollStack() {
		mMessages 		= new ArrayList<>();
		mCounter 		= 0;
		mSeries 		= MiniData.getRandomData(32).to0xString();
	}
	
	public synchronized int getCounter() {
		return mCounter;
	}
	
	public String getSeries() {
		return mSeries;
	}
	
	public synchronized void addMessage(JSONObject zMessage, String zMiniDAPPID) {
		
		//Create a new Poll Message
		PollMessage msg = new PollMessage(mCounter, zMessage, zMiniDAPPID);
		mCounter++;
		
		//Add it to our stack..
		mMessages.add(msg);
		
		//Remove some if stack too big..
		int size = mMessages.size();
		if(size > MAX_MESSAGES) {
			int trim = size - MAX_MESSAGES;
			for(int i=0;i<trim;i++) {
				mMessages.remove(0);
			}
		}
	}
	
	public synchronized PollMessage getMessage(int zMessageCounter, String zMiniDAPPID){		
		
		//Are there any messages
		if(mCounter>zMessageCounter) {
			
			//Cycle through and add them..
			for(PollMessage pmsg : mMessages) {
				
				//Who is it for..
				String zTo	= pmsg.getTo();
				
				//Check the counter
				if(pmsg.getCounter()>=zMessageCounter && (zTo.equals("*") || zTo.equals(zMiniDAPPID))) {
					//Return this message
					return pmsg;
				}
			}
		}
	
		return null;
	}
	
}
