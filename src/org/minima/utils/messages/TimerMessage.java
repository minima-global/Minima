package org.minima.utils.messages;

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
			//Nice and easy - 2 second intervals..
			while( mProcessor.isRunning() && System.currentTimeMillis() < mTimer) {
				Thread.sleep(2000);	
			}
		} catch (InterruptedException e) {}
		
		//And Post..
		if(mProcessor.isRunning()) {
			mProcessor.PostMessage(this);
		} 
	}
}
