package org.minima.system.commands.base;

import java.util.ArrayList;

import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.sendpoll.SendPollManager;
import org.minima.system.sendpoll.SendPollMessage;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class sendpoll extends Command {

	public sendpoll() {
		super("sendpoll","(action:add|list|remove) (uid:) - Send function that is added to a list and polls until complete");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		String action = getParam("action", "add");
		
		JSONObject response = new JSONObject();
		
		SendPollManager sendpoll = Main.getInstance().getSendPoll();
		
		if(action.equals("add")) {
			
			//Get the complete command
			String command = getCompleteCommand();
			
			//Replace sendpoll with send..
			String sendcomm = command.replaceFirst("sendpoll", "send");
			
			//Add this to the Manager
			sendpoll.addSendCommand(sendcomm);
			
			response.put("command", sendcomm);
		
		}else if(action.equals("list")) {
			
			//Get a List of commands..
			ArrayList<SendPollMessage> commands = sendpoll.listCommands();
			
			JSONArray arr = new JSONArray();
			for(SendPollMessage spoll : commands) {
				arr.add(spoll.toJSON());
			}
			
			response.put("commands", arr);
			response.put("total", arr.size());
		
		}else if(action.equals("remove")) {
			
			String uid = getParam("uid");
			
			sendpoll.removeCommand(uid);
			
			response.put("removed", uid);
		}
		
		ret.put("response", response);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new sendpoll();
	}

}
