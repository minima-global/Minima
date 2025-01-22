package org.minima.system.sendpoll;

import java.util.ArrayList;

import org.minima.system.mds.handler.CMDcommand;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;

public class SendPollManager extends MessageProcessor{

	public static final String SENDPOLL_FUNCTION 	= "SENDPOLL_FUNCTION";
	public static long SENDPOLL_TIMER				= 1000 * 30;
	
	public static final String SENDPOLL_LIST 		= "SENDPOLL_CLEAR";
	public static final String SENDPOLL_CLEAR 		= "SENDPOLL_LIST";
	
	ArrayList<SendPollMessage> mSendCommands = new ArrayList<>();
	
	Object mSyncObject = new Object();
	
	public SendPollManager() {
		super("SENDPOLL_MANAGER");
		
		PostTimerMessage(new TimerMessage(SENDPOLL_TIMER, SENDPOLL_FUNCTION));
	}
	
	public void addSendCommand(String zCommand) {
		synchronized (mSyncObject) {
			mSendCommands.add(new SendPollMessage(zCommand));
		}
	}
	
	public ArrayList<SendPollMessage> listCommands() {
		ArrayList<SendPollMessage> copy = new ArrayList<>();
		
		synchronized (mSyncObject) {
			for(SendPollMessage command : mSendCommands) {
				copy.add(command.copy());
			}
		}
		
		return copy;
	}
	
	public void removeCommand(String zUID) {
		ArrayList<SendPollMessage> copy = new ArrayList<>();
		
		synchronized (mSyncObject) {
			for(SendPollMessage command : mSendCommands) {
				if(!command.getUID().equals(zUID)) {
					copy.add(command);
				}
			}
			
			mSendCommands = copy;
		}
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.getMessageType().equals(SENDPOLL_FUNCTION)) {
			
			//Get a copy of the List..
			ArrayList<SendPollMessage> commands = listCommands();
				
			for(SendPollMessage command : commands) {
				
				if(!isRunning()) {
					break;
				}
				
				//Create a Command..
				CMDcommand cmd 	= new CMDcommand("0x00", command.getCommand());
				String result 	= cmd.runCommand();
				
				JSONObject res  = (JSONObject) new JSONParser().parse(result);
				if((boolean)res.get("status")) {
					
					//It worked..remove from the list..
					removeCommand(command.getUID());
				}
				
				//Pause..
				Thread.sleep(1000);
			}
			
			//And do it again
			PostTimerMessage(new TimerMessage(SENDPOLL_TIMER, SENDPOLL_FUNCTION));
		}
	}
}
