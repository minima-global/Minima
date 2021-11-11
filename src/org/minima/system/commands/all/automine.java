package org.minima.system.commands.all;

import org.minima.system.Main;
import org.minima.system.brains.TxPoWMiner;
import org.minima.system.commands.Command;
import org.minima.system.params.GeneralParams;
import org.minima.utils.json.JSONObject;

public class automine extends Command {

	public automine() {
		super("automine","[type:on|off|single] - Simulate traffic");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		String act = (String)getParams().get("type");
		if(act == null) {
			throw new Exception("Must specify type on/off/single");
		}
	
		if(act.equals("single")) {
			//Send 1 mine message
			Main.getInstance().getTxPoWMiner().PostMessage(TxPoWMiner.TXPOWMINER_MINEPULSE);
			
			ret.put("message", "Auto Mining Single TxPoW");
		}else {
		
			boolean on = true;
			if(act.equals("off")) {
				on = false;
			}
			
			GeneralParams.AUTOMINE = on;
			
			ret.put("message", "Auto Mining "+on);
		}
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new automine();
	}

}
