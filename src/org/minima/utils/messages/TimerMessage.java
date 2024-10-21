package org.minima.utils.messages;

public class TimerMessage extends Message {

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
	
	public TimerMessage(long zDelay, Message zMessage) {
		super(zMessage.getMessageType());
		
		//Set the values..
		mContents = zMessage.getAllContents();
		
		//Store for run
		mDelay = zDelay;
		
		//Add the time.
		mTimer = System.currentTimeMillis() + zDelay;
	}
	
	public void setProcessor(MessageProcessor zProcessor) {
		mProcessor = zProcessor;
	}
	
	public MessageProcessor getProcessor() {
		return mProcessor;
	}
	
	public long getTimer() {
		return mTimer;
	}
	
	@Override
	public String toString(){
		long timenow =  System.currentTimeMillis();
		long diff 	 =  mTimer - timenow;
		
		return "Timer:"+mTimer+" Delay:"+diff+" milli Msg:"+super.toString();
	}
}
