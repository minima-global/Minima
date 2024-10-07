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

public class systemcheck extends Command {

	public systemcheck() {
		super("systemcheck","Check system processors..");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"show","action"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
	
		
		JSONObject resp = new JSONObject();
		
		//Get info about each Process Manager
		resp.put("Main", getInfo(Main.getInstance()));
		resp.put("TxPowProcesssor", getInfo(Main.getInstance().getTxPoWProcessor()));
		resp.put("TxPowMiner", getInfo(Main.getInstance().getTxPoWMiner()));
		resp.put("NIOManager", getInfo(Main.getInstance().getNIOManager()));
		resp.put("P2PManager", getInfo(Main.getInstance().getNetworkManager().getP2PManager()));
		resp.put("MDSManager", getInfo(Main.getInstance().getMDSManager()));
		resp.put("SendPollManager", getInfo(Main.getInstance().getSendPoll()));
		resp.put("NotifyManager", getInfo(Main.getInstance().getNotifyManager()));
		
		resp.put("RWLockInfo", MinimaDB.getDB().getRWLockInfo());
		
		resp.put("writelockthread", MinimaDB.getDB().mCurrentWriteLockThread);
		resp.put("writelockthreadstate", MinimaDB.getDB().mCurrentWriteLockState);
		
		resp.put("Shutting Down", Main.getInstance().isShuttingDown());
		
		ret.put("response", resp);

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

		return ret;
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
	
	
	// get a file from the resources folder
    // works everywhere, IDEA, unit test and JAR file.
    private InputStream getFileFromResourceAsStream(String fileName) {

        // The class loader that loaded the class
        ClassLoader classLoader = getClass().getClassLoader();
        InputStream inputStream = classLoader.getResourceAsStream(fileName);

        // the stream holding the file content
        if (inputStream == null) {
            MinimaLogger.log("file not found! " + fileName);
        }
            
        return inputStream;

    }
	
	@Override
	public Command getFunction() {
		return new systemcheck();
	}

	public static void main(String[] zArgs) {
		
		for(int i=0;i<512;i++) {
			
			MiniData data = new MiniData(new BigInteger(Integer.toString(i)));
			
			System.out.println(data.to0xString());
			
		}
		
		
		
	}
}