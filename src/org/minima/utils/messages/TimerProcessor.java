package org.minima.utils.messages;

import java.util.ArrayList;

import org.minima.utils.MinimaLogger;

public class TimerProcessor implements Runnable {

	/**
	 * Static function for all Timed Messages
	 */
	private static TimerProcessor mTimerProcessor = new TimerProcessor();
	public static TimerProcessor getTimerProcessor() {
		return mTimerProcessor;
	}
	public static void  stopTimerProcessor() {
		mTimerProcessor.stop();
	}
	
	/**
	 * Are we running
	 */
	private boolean mRunning;
	
	/**
	 * The main thread
	 */
	Thread mMainThread;
	
	/**
	 * All the timed messages
	 */
	private ArrayList<TimerMessage> mTimerMessages;
	
	private TimerProcessor() {
		mRunning 		= true;
		mTimerMessages 	= new ArrayList<TimerMessage>();
		
		mMainThread = new Thread(this);
		mMainThread.start();
	}
	
	public void stop() {
		mRunning = false;
		
		try {mMainThread.interrupt();}catch (Exception e) {}
	}
	
	public void PostMessage(TimerMessage zMessage) {
		synchronized (mTimerMessages) {
			mTimerMessages.add(zMessage);
		}
	}
	
	@Override
	public void run() {
		while(mRunning) {
			
			//Check the stack for messages..
			synchronized (mTimerMessages) {
				//New list to store the ongoing timers
				ArrayList<TimerMessage> newlist = new ArrayList<TimerMessage>();
				
				//Current time
				long time = System.currentTimeMillis();
				
				//Cycle through all the timers
				for(TimerMessage tm : mTimerMessages) {
					//Get the time..
					if(tm.getTimer()<time) {
						//Who get's it
						MessageProcessor process = tm.getProcessor();
						
						//And Post..
						if(process.isRunning()) {
							process.PostMessage(tm);
						} else {
							MinimaLogger.log("Timer Message NOT run as processor shutdown.. "+tm.toString());
						}
					}else {
						//Keep for next test
						newlist.add(tm);
					}
				}
				
				//Swap lists.
				mTimerMessages = newlist;
			}
			
			//Small sleep.. 
			try {Thread.sleep(1000);} catch (InterruptedException e) {mRunning = false;}
		}
	}

}
