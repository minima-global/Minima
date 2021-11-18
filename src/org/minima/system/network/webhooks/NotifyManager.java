package org.minima.system.network.webhooks;

import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.utils.MinimaLogger;
import org.minima.utils.RPCClient;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;

public class NotifyManager extends MessageProcessor {

	
	public static final String NOTIFY_POST = "NOTIFY_POST";
	
	ArrayList<String> mHooks;
	
	public NotifyManager() {
		super("NOTIFYMANAGER");
		
		//Load the hooks..
		mHooks = MinimaDB.getDB().getUserDB().getWebHooks();
	}

	public void shutDown() {
		
		//Stop this processor
		stopMessageProcessor();
				
		synchronized (mHooks) {
		
			//Save the hooks..
			MinimaDB.getDB().getUserDB().setWebHooks(mHooks);
		}
	}
	
	/**
	 * Post an event to all the listeners
	 */
	public void PostEvent(JSONObject zEvent) {
		Message msg = new Message(NOTIFY_POST);
		msg.addObject("data", zEvent);
		PostMessage(msg);
	}
	
	public ArrayList<String> getAllWebHooks(){
		ArrayList<String> ret= new ArrayList<>();
		
		synchronized (mHooks) {
			for(String hook : mHooks) {
				ret.add(new String(hook));
			}
		}
		
		return ret;
	}
	
	public void addHook(String zHook) {
		synchronized (mHooks) {
			if(!mHooks.contains(zHook) && !zHook.equals("")) {
				mHooks.add(zHook);
			}
		}
	}
	
	public void removeHook(String zHook) {
		synchronized (mHooks) {
			mHooks.remove(zHook);
		}
	}
	
	public void clearHooks() {
		synchronized (mHooks) {
			mHooks.clear();
		}
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
	
		if(zMessage.isMessageType(NOTIFY_POST)) {
			
			//Get the Message
			JSONObject data = (JSONObject) zMessage.getObject("data");
			
			//Convert..
			String postmsg = data.toString();
			
			//Cycle through and Post to each hook..
			ArrayList<String> hooks = getAllWebHooks();
			for(String hook : hooks) {
				
				//Check running..
				if(!isRunning()) {
					return;
				}
				
				try {
				
					//Post it..
					RPCClient.sendPOST(hook, postmsg);
					
				}catch(Exception exc) {
					MinimaLogger.log("ERROR webhook : "+hook+" "+exc);
				}
			}
		}
	}
}
