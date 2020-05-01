package org.minima.utils.messages;

public class TimerMessage extends Message implements Runnable {

	//The timer value
	private long mTimer;
    
	private long mDelay;
    
	private MessageProcessor mProcessor;
	
	public TimerMessage(int zDelay, String zMessageType) {
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
			Thread.sleep(mDelay);
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			//e.printStackTrace();
		}
		
		//And Post..
		mProcessor.PostMessage(this);
	} 

}
