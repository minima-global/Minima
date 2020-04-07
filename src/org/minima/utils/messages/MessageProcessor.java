/**
 * 
 */
package org.minima.utils.messages;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedList;

import org.minima.system.input.InputHandler;
import org.minima.utils.MinimaLogger;

/**
 * @author Spartacus Rex
 *
 */
public abstract class MessageProcessor extends MessageStack implements Runnable{

	/**
	 * Main Thread loop
	 */
    private Thread mMainThread;
    
    /**
     * Are we running
     */
    private boolean mRunning;
    
    /**
	 * All Timer messages in this stack
	 */
	LinkedList<TimerMessage> mTimerMessages;
	
	/**
	 * LOG messages ?
	 */
	protected boolean mLogON = false;
	
	/**
	 * Processor Name
	 */
	String mName;
	
	/**
	 * Constructor
	 */
    public MessageProcessor(String zName){
    	super();
    	
    	mName = zName;
    	
    	mRunning = true;
        
        mTimerMessages = new LinkedList<>();
        
        mMainThread = new Thread(this,zName);
        mMainThread.start();
    }
    
    public void setLOG(boolean zLogON) {
    	mLogON = zLogON;
    }
    
    public boolean isRunning(){
    	return mRunning;
    }
    
    public void stopMessageProcessor(){
        mRunning = false;
    }
    
    public synchronized void PostTimerMessage(TimerMessage zMessage) {
    	mTimerMessages.add(zMessage);
    }
    
    public void run() {
    	//Format the time
    	SimpleDateFormat sdf = new SimpleDateFormat("HH:mm:ss.SSS");

        //Loop while still running
        while(mRunning){
            //Check for valid mnessage
            Message msg = getNextMessage();
            
            //Cycle through available messages
            while(msg != null && mRunning){          
                //Process that message
                try{
                	//Are we logging  ?
                	if(mLogON) {
                		MinimaLogger.log("["+getSize()+"] "+sdf.format(new Date())+" [ "+mMainThread.getName()+" ] \t"+msg);
                	}
                
                	//Process Message
                    processMessage(msg);
                    
                }catch(Exception exc){
//                    MinimaLogger.log("Error processing message : "+msg);
                    exc.printStackTrace();
                    InputHandler.endResponse(msg, false, "SYSTEM ERROR PROCESSING : "+msg+" exception:"+exc);
                } 
                
                //Timer messages..
                checkTimerMessages();
                
                //Are there more messages..
                msg = getNextMessage();
            }
            
            //Check timer..
            checkTimerMessages();
            
            //Small Pause..
            try {Thread.sleep(20);} catch (InterruptedException ex) {}
        }
    }
    
    private synchronized void checkTimerMessages() {
    	//Check Timer Messages
        long timenow = System.currentTimeMillis();
        ArrayList<TimerMessage> remove = new ArrayList<>();
        for(TimerMessage tm : mTimerMessages) {
        	if(timenow > tm.getTimer()) {
        		//It's ready post it
        		PostMessage(tm);
        		
        		//Remove it from the list
        		remove.add(tm);
        	}
        }
        
        //Remove used..
        for(TimerMessage rem : remove) {
        	mTimerMessages.remove(rem);
        }
    }
    
    /**
     * This is the main processing unit, must be overloaded in extends classes
     * @param zMessage The Full Message
     * @throws Exception
     */
    protected abstract void processMessage(Message zMessage) throws Exception;
}

