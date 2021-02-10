package org.minima.system.brains;

import java.util.ArrayList;

import org.minima.objects.Address;
import org.minima.objects.base.MiniData;
import org.minima.system.input.InputHandler;
import org.minima.utils.json.JSONObject;
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
			
		}else if(zMessage.getMessageType().equals(SENDMANAGER_SHUTDOWN)) {
			
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
	
			//Create a Random Reference..
			String ref = MiniData.getRandomData(20).to0xString();
			
			//Create a JSON Object..
			JSONObject sendcommand = new JSONObject();
			sendcommand.put("address", address);
			sendcommand.put("tokenid", tokenid);
			sendcommand.put("amount", amount);
			sendcommand.put("reference", ref);
			
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
			
		}else if(zMessage.getMessageType().equals(SENDMANAGER_CLEAR)) {
			
		
		}else if(zMessage.getMessageType().equals(SENDMANAGER_CHECKPOLL)) {
			//Check for new messages and try to send them..
			
			
			
			//Check again
			PostTimerMessage(new TimerMessage(mPollDelay, SENDMANAGER_CHECKPOLL));
		}
		
	}

}
