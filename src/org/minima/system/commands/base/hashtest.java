package org.minima.system.commands.base;

import org.minima.objects.base.MiniNumber;
import org.minima.system.brains.TxPoWMiner;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONObject;

public class hashtest extends Command {

	public hashtest() {
		super("hashtest","Check the speed of hashing of this device.");
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();

		MiniNumber speed = TxPoWMiner.calculateHashRate();
		
		String spd = speed.div(MiniNumber.MILLION).setSignificantDigits(4)+" MHash/s";
		
		JSONObject resp = new JSONObject();
		resp.put("hashespersec", speed.floor().toString());
		resp.put("speed", spd);
		
		//Add balance..
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new hashtest();
	}

}
