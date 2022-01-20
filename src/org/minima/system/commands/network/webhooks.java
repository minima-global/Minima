package org.minima.system.commands.network;

import java.util.ArrayList;

import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.network.webhooks.NotifyManager;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class webhooks extends Command {

	public webhooks() {
		super("webhooks","(action:list|add|remove|clear) (hook:url) - Add a web hook that is called with Minima events as they happen");
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		//get the data
		String action = getParam("action", "list");
		
		//Get the Notify Manager
		NotifyManager notify = Main.getInstance().getNetworkManager().getNotifyManager();
		
		//The response..
		JSONObject resp = new JSONObject();
		ret.put("response", resp);
		
		if(action.equals("add")) {
			
			String hook = getParam("hook");
			notify.addHook(hook);
			
		}else if(action.equals("remove")) {
			
			String hook = getParam("hook");
			notify.removeHook(hook);
		
		}else if(action.equals("clear")) {
			notify.clearHooks();
			
		}
		
		//List all the current hooks
		ArrayList<String> hooks = notify.getAllWebHooks();
		JSONArray arr = new JSONArray();
		for(String hook : hooks) {
			arr.add(hook);
		}
		
		resp.put("webhooks", arr);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new webhooks();
	}

}
