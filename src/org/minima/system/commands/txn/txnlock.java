package org.minima.system.commands.txn;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.objects.base.MiniNumber;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONObject;

public class txnlock extends Command {

	public txnlock() {
		super("txnlock","(action) (timeout) (unlockdelay) - Gain a lock. Stops multiple txnfunctions occuring simultaneously");
	}
	
	@Override
	public String getFullHelp() {
		return "\txnlock\n"
				+ "\n"
				+ "When creating multiple transactions asynchronously you can ensure each is created one at a time.\n"
				+ "\n"
				+ "action:\n"
				+ "    lock - Gain a lock\n"
				+ "    unlock - Release lock\n"
				+ "    list/blank - Are we locked\n"
				+ "\n"
				+ "timeout:\n"
				+ "    How long to wait for the lock\n"
				+ "\n"
				+ "unlockdelay:\n"
				+ "    Wait this amount of milli after unlock.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "txnlock\n"
				+ "\n"
				+ "txnlock action:lock timeout:10000\n"
				+ "\n"
				+ "txnlock action:unlock unlockdelay:5000\n"
				+ "\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","timeout","unlockdelay"}));
	}
	
	public static boolean mLocked 	= false;
	public static synchronized boolean lockFunction(boolean zEnable){
		
		//Enabling or disabling..
		if(zEnable) {
			if(!mLocked) {
				mLocked = true;
				return true;
			}
			
		}else{
			mLocked = false;
			return true;
		}
		
		return false;
	}
	
	public boolean getLock(long zTimeout) throws InterruptedException{
		
		long delay = 100;
		long counter = 0;
		while(!lockFunction(true)) {
			Thread.sleep(delay);
			counter+=delay;
			
			if(zTimeout!=0) {
				if(counter>zTimeout) {
					return false;
				}
			}
		}
		
		return true;
	}
	
	public void unlock() throws InterruptedException {
		lockFunction(false);
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();

		String action = getParam("action","list");
		
		//10 second default timer
		long timeout 	 = getNumberParam("timeout", new MiniNumber(20000)).getAsLong();
		long unlockdelay = getNumberParam("unlockdelay", MiniNumber.ZERO).getAsLong();
		
		JSONObject resp = new JSONObject();
		if(action.equals("lock")) {
			boolean success = getLock(timeout);
			resp.put("success", success);
			resp.put("locked", true);
		}else if(action.equals("unlock")) {
			
			//Is there a delay
			if(unlockdelay!=0) {
				Thread.sleep(unlockdelay);
			}
			
			unlock();
			
			resp.put("success", true);
			resp.put("locked", false);
		}else {
			resp.put("locked", mLocked);
		}

		ret.put("response", resp);
		return ret;
	}

	@Override
	public Command getFunction() {
		return new txnlock();
	}

}
