package org.minima.system.brains;

import java.util.ArrayList;

import org.minima.objects.Address;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.input.InputHandler;
import org.minima.system.network.commands.CMD;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;

public class SendManager extends MessageProcessor {
	
	public static final String SENDMANAGER_INIT     = "SENDMANAGER_INIT";
	public static final String SENDMANAGER_SHUTDOWN = "SENDMANAGER_SHUTDOWN";
	
	public static final String SENDMANAGER_ADD   = "SENDMANAGER_ADD";
	public static final String SENDMANAGER_LIST  = "SENDMANAGER_LIST";
	public static final String SENDMANAGER_CLEAR = "SENDMANAGER_CLEAR";
	
	public static final String SENDMANAGER_CHECKPOLL = "SENDMANAGER_CHECKPOLL";
	public static final long mPollDelay = 10000;
	
	ArrayList<JSONObject> mSendCommands= new ArrayList<>();
	
	public SendManager() {
		super("SEND_MANAGER");
		
		//Start Polling..
		PostTimerMessage(new TimerMessage(mPollDelay, SENDMANAGER_CHECKPOLL));
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		if(zMessage.getMessageType().equals(SENDMANAGER_INIT)) {
			//Load DB ?
			
		}else if(zMessage.getMessageType().equals(SENDMANAGER_SHUTDOWN)) {
			//Clean up..
			//..
			
			//Stop this processor..
			stopMessageProcessor();
			
		}else if(zMessage.getMessageType().equals(SENDMANAGER_ADD)) {
			//How much to who ?
			String address 	= zMessage.getString("address");
			if(address.startsWith("0x")) {
				//It's a regular HASH address
				address = new MiniData(address).to0xString();
			}else if(address.startsWith("Mx")) {
				//It's a Minima Address!
				address = Address.convertMinimaAddress(address).to0xString();
			}
			
			String tokenid 	   	= new MiniData(zMessage.getString("tokenid")).to0xString();
			String amount  		= zMessage.getString("amount");
			String state  		= zMessage.getString("state");
			
			//Check..
			MiniNumber amt = new MiniNumber(amount);
			
			//Create a Random Reference..
			String ref = MiniData.getRandomData(20).to0xString();
			
			//Create a JSON Object..
			JSONObject sendcommand = new JSONObject();
			sendcommand.put("address", address);
			sendcommand.put("tokenid", tokenid);
			sendcommand.put("amount", amount);
			sendcommand.put("state", state);
			sendcommand.put("reference", ref);
			sendcommand.put("attempts", (int)0);
			
			//Add it to the poll list..
			mSendCommands.add(sendcommand);
			
			//How many now..
			int len = mSendCommands.size();
			
			//And That's it..
			JSONObject resp = InputHandler.getResponseJSON(zMessage);
			resp.put("command", sendcommand);
			resp.put("total", len);
			
			InputHandler.endResponse(zMessage, true, "Send added to list");
			
		}else if(zMessage.getMessageType().equals(SENDMANAGER_LIST)) {
			JSONObject resp = InputHandler.getResponseJSON(zMessage);
			
			MiniNumber totalsend = MiniNumber.ZERO;
			
			JSONArray allcommands = new JSONArray();
			for(JSONObject command : mSendCommands) {
				allcommands.add(command);
				
				//Add to the total..
				MiniNumber amount = new MiniNumber((String)command.get("amount"));
				totalsend = totalsend.add(amount);
			}
			resp.put("commands", allcommands);
			resp.put("totalcommands", (int)allcommands.size());
			resp.put("totalsend", totalsend.toString());
			
			InputHandler.endResponse(zMessage, true, "All waiting commands");
			
		}else if(zMessage.getMessageType().equals(SENDMANAGER_CLEAR)) {
			//What Reference..
			String ref = zMessage.getString("reference");
					
			if(ref.equals("all")) {
				//Clear all the commands
				mSendCommands.clear();
				
				InputHandler.endResponse(zMessage, true, "All commands cleared");
				
			}else {
				boolean found = false;
				
				//Only clear the one referenced command..
				ArrayList<JSONObject> remainingCommands= new ArrayList<>();
				for(JSONObject command : mSendCommands) {
					String cmdref = (String) command.get("reference");
					if(found || !cmdref.equals(ref)) {
						remainingCommands.add(command);
					}else {
						found = true;
					}
				}
				
				mSendCommands = remainingCommands;
				
				if(found) {
					InputHandler.endResponse(zMessage, true, "Command cleared");
				}else {
					InputHandler.endResponse(zMessage, false, "Reference not found");
				}
			}
			
			
		}else if(zMessage.getMessageType().equals(SENDMANAGER_CHECKPOLL)) {
			//Keep those that fail..
			ArrayList<JSONObject> remainingCommands= new ArrayList<>();
			
			//Check for new messages and try to send them..
			for(JSONObject command : mSendCommands) {
				String address = (String) command.get("address");
				String amount  = (String) command.get("amount");
				String token   = (String) command.get("tokenid");
				String state   = (String) command.get("state");
				
				
				//Now run this command..
				CMD cmd = new CMD("send "+amount+" "+address+" "+token+" \""+state+"\"");
				
				//Run it.. wait for it to finish
				cmd.run();

				//Get the Response..
				String resp = cmd.getFinalResult();
				
				//Convert to JSON
				JSONObject jsonresp = (JSONObject)(new JSONParser().parse(resp));
				
				//Was it a success..
				boolean status = (boolean) jsonresp.get("status");
				
				if(!status) {
					MinimaLogger.log(resp);
				}
				
				//Get the status
				if(status) {
					//Success ..it's done
					//..
				}else {
					//Fail..
					int attempt = (int) command.get("attempts");
					attempt++;
					command.put("attempts", attempt);
					
					//Add to the remaining..
					remainingCommands.add(command);
				}
				
				//Wait a second..
				Thread.sleep(250);
			}
			
			//Switch..
			mSendCommands = remainingCommands;
			
			//Check again
			PostTimerMessage(new TimerMessage(mPollDelay, SENDMANAGER_CHECKPOLL));
		}
		
	}

}
