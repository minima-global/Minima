/**
 * 
 */
package org.minima.utils.messages;

import java.util.LinkedList;

/**
 * @author Spartacus Rex
 *
 */
public class MessageStack{
    
	/**
	 * All messages in this stack
	 */
	private LinkedList<Message> mMessages;
	
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
    public synchronized void PostMessage(Message zMessage){
        mMessages.add(zMessage);
    }
    
    /**
     * Get the first message on the stack, if there is one
     */
    protected synchronized Message getNextMessage(){
        if(!mMessages.isEmpty()){
            //Get the first message
            Message msg = mMessages.getFirst();
            
            //Remove it from the list
            mMessages.remove(msg);
            
            //Return it.
            return msg;
        }
        
        return null;
    }
    
    /**
     * Get the first message on the stack, if there is one
     */
    protected synchronized int getSize(){
        return mMessages.size();
    }
}
