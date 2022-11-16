/**
 * 
 */
package org.minima.utils.messages;

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
     * Have we finbished shutting down
     */
    private boolean mShutDownComplete;
    
	/**
	 * LOG messages ?
	 */
	static private boolean mTrace 		= false;
	static private String mTraceFilter 	= "";
	
	public static void setTrace(boolean zTrace, String zTraceFilter) {
		mTrace 			= zTrace;
		mTraceFilter 	= zTraceFilter;
	}
	
	/**
	 * Processor Name
	 */
	String mName;
	
	/**
	 * Constructor
	 */
    public MessageProcessor(String zName){
    	super();
    	
    	mName 				= zName;
    	mRunning 			= true;
    	mShutDownComplete 	= false;
    	
    	mMainThread = new Thread(this,zName);
        mMainThread.start();
    }
    
    public void setFullLogging(boolean zLogON, String zTraceFilter) {
    	mTrace 			= zLogON;
    	mTraceFilter 	= zTraceFilter;
    }
    
    public boolean isTrace() {
    	return mTrace;
    }
    
    public String getTraceFilter() {
    	return mTraceFilter;
    }
    
    public boolean isRunning(){
    	return mRunning;
    }
    
    public boolean isShutdownComplete() {
    	return mShutDownComplete;
    }
    
    public void waitToShutDown(boolean zUseLimit) {
    	long timewaited = 0;
    	while(!isShutdownComplete()) {
			try {Thread.sleep(250);} catch (InterruptedException e) {}
			timewaited +=250;
			if(timewaited>10000) {
				timewaited = 0;
				MinimaLogger.log("Failed to shutdown in 10 secs for "+mName);
				if(zUseLimit) {
					break;
				}
			}
		}
    }
    
    public void stopMessageProcessor(){
        mRunning = false;
        
        //Wake it up if is locked..
        notifyLock();
    }
    
    public void PostTimerMessage(TimerMessage zMessage) {    	
    	//Set this is the processor..
    	zMessage.setProcessor(this);
    	
    	//Post it on the TimerProcessor
    	TimerProcessor.getTimerProcessor().PostMessage(zMessage);
    }
    
    public void run() {
    	
    	if(mTrace) {
        	MinimaLogger.log("["+mMainThread.getName()+"] (stack:"+getSize()+") START", false);
        }
    	
    	//Loop while still running
        while(mRunning){
            //Check for valid mnessage
            Message msg = getNextMessage();
            
            //Cycle through available messages
            while(msg != null && mRunning){          
                //Process that message
                try{
                	//Are we logging  ?
                	long timenow = 0;
                	if(mTrace) {
                		timenow = System.currentTimeMillis();
                	}
                
                	//Process Message
                    processMessage(msg);
                
                    if(mTrace) {
                    	long timediff = System.currentTimeMillis() - timenow;
                    	String tracemsg = msg.toString();
                		if(tracemsg.contains(mTraceFilter)) {
                			MinimaLogger.log("["+mMainThread.getName()+"] (stack:"+getSize()+") time:"+timediff+" \t"+msg, false);
                		}
                    }
                    
                }catch(Error noclass){
                	MinimaLogger.log("**SERIOUS SETUP ERROR "+msg.getMessageType()+" "+noclass.toString());
                	
                }catch(Exception exc){
                	MinimaLogger.log("MESSAGE PROCESSING ERROR @ "+msg.getMessageType());
                	MinimaLogger.log(exc);
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

        if(mTrace) {
        	MinimaLogger.log("["+mMainThread.getName()+"] (stack:"+getSize()+") SHUTDOWN", false);
        }
        
        //All done..
        mShutDownComplete = true;
    }
    
    /**
     * This is the main processing unit, must be overloaded in extends classes
     * @param zMessage The Full Message
     * @throws Exception
     */
    protected abstract void processMessage(Message zMessage) throws Exception;
}

