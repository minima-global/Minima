/**
 * 
 */
package org.minima.utils.messages;

import java.text.SimpleDateFormat;
import java.util.Date;

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
    
    	mMainThread = new Thread(this,zName);
//    	mMainThread.setDaemon(true);
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
        
        //Wake it up if is locked..
        notifyLock();
    }
    
    public void PostTimerMessage(TimerMessage zMessage) {    	
    	//Set this is the processor..
    	zMessage.setProcessor(this);
    	
    	//Create a Thread that fires//
    	Thread timer = new Thread(zMessage);
    	timer.setDaemon(true);
    	timer.start();
    }
    
    public void run() {
//    	MinimaLogger.log("MESSAGE_PROCESSOR "+mName+" STARTED");
    	
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
                    exc.printStackTrace();
                    InputHandler.endResponse(msg, false, "SYSTEM ERROR PROCESSING : "+msg+" exception:"+exc);
                } 
                
                //Are there more messages..
                msg = getNextMessage();
            }
            
            //Wait.. for a notify.. 
            try {
            	synchronized (mLock) {
            		//Last check.. inside the LOCK
            		if(!isNextMessage() && mRunning) {
            			//Wait for a message to be posted on the stack
            			mLock.wait();	
            		}
				}
			} catch (InterruptedException e) {
				MinimaLogger.log("MESSAGE_PROCESSOR "+mName+" INTERRUPTED");
			}
        }

//        MinimaLogger.log("MESSAGE_PROCESSOR "+mName+" STOPPED");
    }
    
    /**
     * This is the main processing unit, must be overloaded in extends classes
     * @param zMessage The Full Message
     * @throws Exception
     */
    protected abstract void processMessage(Message zMessage) throws Exception;
}

