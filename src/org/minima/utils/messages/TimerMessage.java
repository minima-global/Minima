package org.minima.utils.messages;

public class TimerMessage extends Message {

	//The timer value
	private long mTimer;
    
	public TimerMessage(int zDelay, String zMessageType) {
		super(zMessageType);
		
		//Add the time.
		mTimer = System.currentTimeMillis() + zDelay;
	}
	
	public long getTimer() {
		return mTimer;
	} 

}
