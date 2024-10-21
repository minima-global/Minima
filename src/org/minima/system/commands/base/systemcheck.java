package org.minima.system.commands.base;

import java.io.InputStream;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.objects.base.MiniData;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;
import org.minima.utils.messages.TimerProcessor;

public class systemcheck extends Command {

	public systemcheck() {
		super("systemcheck","Check system processors..");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"processor","action"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
	
		
		JSONObject resp = new JSONObject();
		
		String action=getParam("action","list");
		
		if(action.equals("list")) {
			//Get info about each Process Manager
			resp.put("Main", getInfo(Main.getInstance()));
			resp.put("TxPowProcesssor", getInfo(Main.getInstance().getTxPoWProcessor()));
			resp.put("TxPowMiner", getInfo(Main.getInstance().getTxPoWMiner()));
			resp.put("NIOManager", getInfo(Main.getInstance().getNIOManager()));
			resp.put("P2PManager", getInfo(Main.getInstance().getNetworkManager().getP2PManager()));
			resp.put("MDSManager", getInfo(Main.getInstance().getMDSManager()));
			resp.put("SendPollManager", getInfo(Main.getInstance().getSendPoll()));
			resp.put("NotifyManager", getInfo(Main.getInstance().getNotifyManager()));
			
			//The Timer Processor..
			TimerProcessor tp = TimerProcessor.getTimerProcessor();
			resp.put("TimerProcessor", tp.getSize());
			
			resp.put("RWLockInfo", MinimaDB.getDB().getRWLockInfo());
			
			resp.put("writelockthread", MinimaDB.getDB().mCurrentWriteLockThread);
			resp.put("writelockthreadstate", MinimaDB.getDB().mCurrentWriteLockState);
			
			resp.put("Shutting Down", Main.getInstance().isShuttingDown());
			
			//Check the Read Write Lock..
			MinimaLogger.log("Posting Checker Call in TxPoWProcessor..");
			Main.getInstance().getTxPoWProcessor().postCheckCall();

			//Post a message to the Mina thread also..
			MinimaLogger.log("Posting Checker Call in Main..");
			Main.getInstance().PostMessage(Main.MAIN_CALLCHECKER);
			
			//And a timer message
			Message msg = new Message(Main.MAIN_CALLCHECKER);
			msg.addBoolean("timer", true);
			TimerMessage timed = new TimerMessage(1000, msg);
			MinimaLogger.log("Posting TIMED Checker Call in Main..");
			Main.getInstance().PostTimerMessage(timed);
		
		}else if(action.equals("details")) {
			
			String proc = getParam("processor");
			
			if(proc.equalsIgnoreCase("p2pmanager")) {
				printDetails(Main.getInstance().getNetworkManager().getP2PManager());
				
			}else if(proc.equalsIgnoreCase("niomanager")) {
				printDetails(Main.getInstance().getNIOManager());
			
			}else if(proc.equalsIgnoreCase("main")) {
				printDetails(Main.getInstance());
			
			}else if(proc.equalsIgnoreCase("txpowprocessor")) {
				printDetails(Main.getInstance().getTxPoWProcessor());
			
			}else if(proc.equalsIgnoreCase("txpowminer")) {
				printDetails(Main.getInstance().getTxPoWMiner());
			
			}else if(proc.equalsIgnoreCase("mdsmanager")) {
				printDetails(Main.getInstance().getMDSManager());
			
			}else if(proc.equalsIgnoreCase("notifymanager")) {
				printDetails(Main.getInstance().getNotifyManager());
			
			}else if(proc.equalsIgnoreCase("senpollmanager")) {
				printDetails(Main.getInstance().getSendPoll());
			
			}else if(proc.equalsIgnoreCase("timerprocessor")) {
				MinimaLogger.log("Processor Details  : TimerProcessor",false);
				TimerProcessor.getTimerProcessor().printAllMessages();
			}
			
			resp.put("details", "Sent to Minima Log");
		}
		
		ret.put("response", resp);

		return ret;
	}
	
	public void printDetails(MessageProcessor zProc) {
		MinimaLogger.log("Processor Details : "+zProc.getName(),false);
		zProc.printAllMessages();
	}
	
	public JSONObject getInfo(MessageProcessor zProc) {
		JSONObject ret = new JSONObject();
		ret.put("stack", zProc.getSize());
		
		Message lst = zProc.getLastMessage();
		if(lst == null) {
			ret.put("lastmessage", null);
		}else {
			ret.put("lastmessage", lst.toString());
		}
		return ret;
	}
	
		
	@Override
	public Command getFunction() {
		return new systemcheck();
	}
}