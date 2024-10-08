/**
 * 
 */
package org.minima.utils.messages;

import java.util.ArrayList;
import java.util.LinkedList;

import org.minima.utils.MinimaLogger;

/**
 * Thread Safe Message Stack
 * 
 * @author Spartacus Rex
 *
 */
public class MessageStack{
    
	/**
	 * All messages in this stack
	 */
	private LinkedList<Message> mMessages;
	
	/**
	 * The LOCK Object
	 */
	protected Object mLock = new Object();
	
	
	/**
	 * Main Constructor
	 */
    public MessageStack(){
        //Create the Stack where messages are posted
        mMessages = new LinkedList<Message>();
    }
    
    /**
     * Utility function to add a message given just the message type
     */
    public void PostMessage(String zMessage){
        PostMessage(new Message(zMessage)); 
    }
    
    /**
     * Synchronized function to add a Message onto the Stack
     */
    public void PostMessage(Message zMessage){
    	//Multiple threads can call this..
    	synchronized(mMessages) {
    		mMessages.add(zMessage);	
    	}
    	
    	//There is something in the stack
        notifyLock();
    }
    
    protected void notifyLock(){
    	//Wake the Thread..
        synchronized (mLock) {
    		mLock.notifyAll();	
		}
    }
    
    
    /**
     * Is there a next message!
     * @return
     */
    public boolean isNextMessage(){
    	boolean avail;
    	
    	synchronized (mMessages) {
			avail = !mMessages.isEmpty();
		}
    	
    	return avail;
    }
        
    /**
     * Get the first message on the stack, if there is one
     */
    protected Message getNextMessage(){
    	Message nxtmsg = null;
    	
    	synchronized (mMessages) {
    		if(!mMessages.isEmpty()){
                //Get the first message
                nxtmsg = mMessages.getFirst();
                
                //Remove it from the list
                mMessages.remove(nxtmsg);
    		}	
		}
    	
        return nxtmsg;
    }
    
    /**
     * Get the first message on the stack, if there is one
     */
    public int getSize(){
        int len;
        synchronized (mMessages) {
			len = mMessages.size();
		}
    	return len;
    }
    
    /**
     * Clear ALL messages
     */
    public void clear(){
        synchronized (mMessages) {
			mMessages.clear();
		}
    }
    
    public void clearExcept(ArrayList<String> zExclude){
        synchronized (mMessages) {

        	LinkedList<Message> newMessages = new LinkedList<>();
        	
        	for(Message msg : mMessages) {
				if(!zExclude.contains(msg.getMessageType())) {
					newMessages.add(msg);
				}
			}
        	
        	mMessages = newMessages;
		}
    }
    
    /**
     * Print out the stack - NO NOTIFY
     */
    public void printAllMessages() {
    	synchronized (mMessages) {
    		int count=0;
			for(Message msg : mMessages) {
				MinimaLogger.log("MSG_PROC ["+count+"] : "+msg.toString(),false);
				count++;
			}
		}
    }
}
