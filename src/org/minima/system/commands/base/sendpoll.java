package org.minima.system.commands.base;

import java.util.ArrayList;
import java.util.Arrays;

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
	public String getFullHelp() {
		return "\nsendpoll\n"
				+ "\n"
				+ "Send function that adds 'send' commands to a list and polls every 30 seconds until the return status is 'true'.\n"
				+ "\n"
				+ "Accepts the same parameters as the 'send' function.\n"
				+ "\n"
				+ "action: (optional)\n"
				+ "    list : list all the 'send' commands in the polling list.\n"
				+ "    remove : remove a 'send' command from the polling list.\n"
				+ "\n"
				+ "uid: (optional)\n"
				+ "    The uid of a 'send' command you wish to remove from the polling list. Use with 'action:remove'.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "sendpoll address:0xFF.. amount:10\n"
				+ "\n"
				+ "sendpoll address:0xFF.. amount:10 tokenid:0xFED5.. burn:0.1\n"
				+ "\n"
				+ "sendpoll action:list\n"
				+ "\n"
				+ "sendpoll action:remove uid:0x..\n";				
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","uid",
				"address","amount","multi","tokenid","state","burn","split","debug","dryrun","mine"}));
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