package org.minima.utils.messages;

import org.minima.utils.MinimaLogger;

public class TimerMessage extends Message implements Runnable {

	//The timer value
	private long mTimer;
    
	private long mDelay;
    
	private MessageProcessor mProcessor;
	
	public TimerMessage(long zDelay, String zMessageType) {
		super(zMessageType);
		
		//Store for run
		mDelay = zDelay;
		
		//Add the time.
		mTimer = System.currentTimeMillis() + zDelay;
	}
	
	public void setProcessor(MessageProcessor zProcessor) {
		mProcessor = zProcessor;
	}
	
	public long getTimer() {
		return mTimer;
	}

	@Override
	public void run() {
		try {
			//Safety check..
			long timenow = System.currentTimeMillis();
			
			//Sleep until ready..
			Thread.sleep(mDelay);
		
			//Safety check
			long timeafter = System.currentTimeMillis();
			long diff = timeafter-timenow;
			if(diff > mDelay*2) {
				MinimaLogger.log("Timer Message Delay OVER twice as long as requested.. "+diff+"/"+mDelay);
			}
			
//			//Nice and easy - 2 second intervals..
//			while( mProcessor.isRunning() && System.currentTimeMillis() < mTimer) {
//				Thread.sleep(2000);	
//			}
			
		} catch (InterruptedException e) {
			MinimaLogger.log("Timer Message Interrupted.. "+toString());
			return;
		}
		
		//And Post..
		if(mProcessor.isRunning()) {
			mProcessor.PostMessage(this);
		} else {
			MinimaLogger.log("Timer Message NOT run as processor shutdown.. "+toString());
		}
	}
}
