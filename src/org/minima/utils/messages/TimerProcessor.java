package org.minima.utils.messages;

import java.util.LinkedList;

import org.minima.utils.MinimaLogger;

public class TimerProcessor implements Runnable {

	/**
	 * Static functiond for all
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
	 * All the timed messages
	 */
	private LinkedList<TimerMessage> mTimerMessages;
	
	public TimerProcessor() {
		mRunning = true;
		mTimerMessages = new LinkedList<TimerMessage>();
		
		Thread runner = new Thread(this);
		runner.start();
	}
	
	public void stop() {
		mRunning = false;
	}
	
	public void PostMessage(TimerMessage zMessage) {
//		MinimaLogger.log("Time Message : "+zMessage.toString());
		synchronized (mTimerMessages) {
			mTimerMessages.add(zMessage);
		}
	}
	
	@Override
	public void run() {
		while(mRunning) {
			
			//Check the stack for messages..
			synchronized (mTimerMessages) {
				
//				MinimaLogger.log("Timers : "+mTimerMessages.size());
				
				//New list to store the ongoing timers
				LinkedList<TimerMessage> newlist = new LinkedList<TimerMessage>();
				
				//Current time
				long time = System.currentTimeMillis();
				
				//Cycle through all the timers
				for(TimerMessage tm : mTimerMessages) {
					//Get the time..
					if(tm.getTimer()<time) {
//						MinimaLogger.log("Process Time Message : "+tm.toString());
						
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
			
			//Small sleep.. 10s..
			try {
				Thread.sleep(10000);
			} catch (InterruptedException e) {
				MinimaLogger.log(e);
				mRunning = false;
			}
		}
	}

}
