package org.minima.system.network.webhooks;

import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.system.Main;
import org.minima.utils.MinimaLogger;
import org.minima.utils.RPCClient;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageListener;
import org.minima.utils.messages.MessageProcessor;

public class NotifyManager extends MessageProcessor {

	/**
	 * Post a message to all listeners
	 */
	public static final String NOTIFY_POST = "NOTIFY_POST";
	
	/**
	 * RPC listeners
	 */
	ArrayList<String> mHooks;
	
	public boolean WEBHOOKS_ERROR_LOGS = false;
	
	public NotifyManager() {
		super("NOTIFYMANAGER");
		
		//Load the hooks..
		mHooks = MinimaDB.getDB().getUserDB().getWebHooks();
	}

	public void shutDown() {
		//Stop this processor
		stopMessageProcessor();
	}
	
	/**
	 * Post an event to all the listeners
	 */
	public void PostEvent(JSONObject zEvent) {
		Message msg = new Message(NOTIFY_POST);
		msg.addObject("notify", zEvent);
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
			
			//Save the hooks..
			MinimaDB.getDB().getUserDB().setWebHooks(mHooks);
		}
	}
	
	public void removeHook(String zHook) {
		synchronized (mHooks) {
			mHooks.remove(zHook);
			
			//Save the hooks..
			MinimaDB.getDB().getUserDB().setWebHooks(mHooks);
		}
	}
	
	public void clearHooks() {
		synchronized (mHooks) {
			mHooks.clear();
			
			//Save the hooks..
			MinimaDB.getDB().getUserDB().setWebHooks(mHooks);
		}
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
	
		if(zMessage.isMessageType(NOTIFY_POST)) {
			
			//Get the Message
			JSONObject notify 	= (JSONObject) zMessage.getObject("notify");
			String event 		= notify.getString("event");
			
			//Is some one listening directly
			MessageListener minilistener = Main.getMinimaListener();
			if(minilistener != null) {
				try {
					minilistener.processMessage(zMessage);
				}catch(Exception exc) {
					MinimaLogger.log(exc+ " : "+zMessage.toString());
				}
			}
			
			//Convert..
			String postmsg = notify.toString();
			
			//Cycle through and Post to each hook..
			ArrayList<String> hooks = getAllWebHooks();
			for(String hook : hooks) {
				
				int index = hook.indexOf("#");
				if(index == -1) {
					return;
				}
				
				String filter 	= hook.substring(0, index); 
				String webhook 	= hook.substring(index+1, hook.length());
				
				//Check running..
				if(!isRunning()) {
					return;
				}
				
				try {
				
					if(filter.equals("") || event.contains(filter)) {
						//Post it..
						RPCClient.sendPOST(webhook, postmsg);
					}
					
				}catch(Exception exc) {
					if(WEBHOOKS_ERROR_LOGS) {
						MinimaLogger.log("ERROR webhook : "+hook+" "+exc, false);
					}
				}
			}
		}
	}
}
