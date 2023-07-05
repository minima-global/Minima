package org.minima.system.commands.txn;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.objects.StateVariable;
import org.minima.objects.Transaction;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.json.JSONObject;

public class txnlock extends Command {

	public txnlock() {
		super("txnlock","[id:] [port:] [value:] - Add a state variable");
	}
	
	@Override
	public String getFullHelp() {
		return "\ntxnstate\n"
				+ "\n"
				+ "Add a state variable to a transaction.\n"
				+ "\n"
				+ "id:\n"
				+ "    The id of the transaction.\n"
				+ "\n"
				+ "port:\n"
				+ "    Port number of the state variable, from 0-255.\n"
				+ "\n"
				+ "value:\n"
				+ "    Value for the state variable.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "txnstate id:multisig port:0 value:0xFED5..\n"
				+ "\n"
				+ "txnstate id:multisig port:1 value:100 \n"
				+ "\n"
				+ "txnstate id:multisig port:1 value:\"string\" \n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","timeout"}));
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
		
		long counter = 0;
		while(!lockFunction(true)) {
			Thread.sleep(50);
			counter+=50;
			if(counter>zTimeout) {
				return false;
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
		long timeout = getNumberParam("timeout", new MiniNumber(5000)).getAsLong();
		
		JSONObject resp = new JSONObject();
		if(action.equals("lock")) {
			boolean success = getLock(timeout);
			resp.put("success", success);
			resp.put("locked", true);
		}else if(action.equals("unlock")) {
			unlock();
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
