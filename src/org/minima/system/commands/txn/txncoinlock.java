package org.minima.system.commands.txn;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.system.commands.Command;
import org.minima.utils.json.JSONObject;

public class txncoinlock extends Command {

	public txncoinlock() {
		super("txncoinlock","(action:) - Lock coins used in un-broadcast transactions");
	}
	
	@Override
	public String getFullHelp() {
		return "\txncoinlock\n"
				+ "\n"
				+ "Lock coins used in un-broadcast transactions.\n"
				+ "\n"
				+ "\n"
				+ "action: (optional)\n"
				+ "    lock - lock coins.\n"
				+ "    unlock - unlock coins.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "txncoinlock\n"
				+ "\n"
				+ "txncoinlock action:lock\n"
				+ "\n"
				+ "txncoinlock action:unlock\n"
				+ "\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();

		//Lock or unlock
		String action = getParam("action","");
		
		if(action.equals("lock")) {
			txnaddamount.enableCoinLock(true);
		}else if(action.equals("unlock")) {
			txnaddamount.enableCoinLock(false);
		}
		
		JSONObject resp = new JSONObject();
		resp.put("coinslocked", txnaddamount.isCoinLockEnabled());
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new txncoinlock();
	}

}
