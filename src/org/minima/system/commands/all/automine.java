package org.minima.system.commands.all;

import org.minima.system.Main;
import org.minima.system.brains.TxPoWMiner;
import org.minima.system.commands.Command;
import org.minima.system.params.GeneralParams;
import org.minima.utils.json.JSONObject;

public class automine extends Command {

	public automine() {
		super("automine","[enable:true|false|single] - Simulate traffic");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		String enable = getParam("enable","");
		
		if(enable.equals("single")) {
			//Send 1 mine message
			Main.getInstance().getTxPoWMiner().PostMessage(TxPoWMiner.TXPOWMINER_MINEPULSE);
			
			ret.put("message", "Mining Single PULSE TxPoW");
		
		}else if(enable.equals("true")) {
			GeneralParams.AUTOMINE = true;
			
		}else if(enable.equals("false")) {
			GeneralParams.AUTOMINE = false;
			
		}
		
		JSONObject mine = new JSONObject();
		mine.put("enabled", GeneralParams.AUTOMINE);
		
		ret.put("response", mine);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new automine();
	}

}
