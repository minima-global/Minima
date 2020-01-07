/**
 * 
 */
package org.minima.utils.messages;

import java.util.HashMap;
import java.util.Set;

/**
 * Message class
 * 
 * @author Spartacus Rex
 *
 */
public class Message {

	/**
	 * Message Type
	 * 
	 * ALL messages have a message type.
	 */
	private String mMessageType;
	
	/**
	 * The Contents of the Message
	 */
	private HashMap<String, Object> mContents;
	
	/**
	 * Main constructor
	 * 
	 * @param zMessageType The type of message this is
	 */
	public Message(String zMessageType){
		//Store the Message Type
		mMessageType = zMessageType;
		
		//Create the message contents container
		mContents = new HashMap<String, Object>();
	}
	
	/**
	 * Must set the message type later on
	 */
	public Message(){
		//Store the Message Type
		mMessageType = "";
		
		//Create the message contents container
		mContents = new HashMap<String, Object>();
	}
	
	/**
	 * Set the message type
	 * 
	 * @param zMessageType
	 */
	public void setMessageType(String zMessageType) {
		mMessageType = zMessageType;
	}
	
	/**
	 * Get the type of message
	 * @return
	 */
	public String getMessageType(){ 
		return mMessageType;
	}
	
	/**
	 * Check if the message is of a certain type
	 * 
	 * @param zMessageType
	 * @return
	 */
	public boolean isMessageType(String zMessageType){
		return (zMessageType.equals(mMessageType));
	}
	
	/**
	 * Add an Object to the Message
	 * 
	 * @param zName The Name of this Object
	 * @param zObject The Object
	 * @return This Message
	 */
	public Message addObject(String zName, Object zObject){
		//Add to the contents
		mContents.put(zName, zObject);
		
		//return this message
		return this;
	}
	
	public Message addInt(String zName, int zValue) {
		return addObject(zName, new Integer(zValue));
	}
	
	public Message addString(String zName, String zValue) {
		return addObject(zName, new String(zValue));
	}
	
	public Message addBoolean(String zName, boolean zValue) {
		return addObject(zName, new Boolean(zValue));
	}
	
	public boolean exists(String zVariable) {
		return ( getObject(zVariable) != null );
	}
	
	/**
	 * Retrieve an Object from the Message
	 * 
	 * @param zName The Name of the Object
	 * @return The Object or NULL if not found
	 */
	public Object getObject(String zName){
		return mContents.get(zName);
	}
	
	/**
	 * Get a boolean value
	 * @param zName
	 * @return
	 */
	public boolean getBoolean(String zName){
		Object bool = mContents.get(zName);
		
		if(bool == null) {
			return false;
		}
		
		return ((Boolean)bool).booleanValue();
	}
	
	/**
	 * Get an Integer value
	 * @param zName
	 * @return
	 */
	public int getInteger(String zName){
		return ((Integer)mContents.get(zName)).intValue();
	}
	
	/**
	 * Get a String value
	 * @param zName
	 * @return
	 */
	public String getString(String zName){
		return ((String)mContents.get(zName));
	}
	
	@Override
	public String toString(){
		Set<String> keys = mContents.keySet();
		
		String contents = "";
		for(String key : keys){
			Object obj = mContents.get(key);
			
			if(obj != null) {
				contents += key+":"+mContents.get(key).toString()+", ";
			}else {
				contents += key+":null, ";
			}
		}
		
		return "[ "+mMessageType+", "+contents+" ]";
	}
}

