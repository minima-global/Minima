package org.minima.system.commands.base;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.brains.TxPoWMiner;
import org.minima.system.commands.Command;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;

public class hashtest extends Command {

	public hashtest() {
		super("hashtest","(amount:) - Check the speed of hashing of this device. Defaults to 1 million hashes");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"amount"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();

		//How many hashes to perform
		MiniNumber hashes = getNumberParam("amount", MiniNumber.MILLION);
		
		long timenow 	 = System.currentTimeMillis();
		MiniNumber speed = TxPoWMiner.calculateHashSpeed(hashes);
		long timediff 	 = System.currentTimeMillis() - timenow;
		
		MinimaLogger.log("Speed : "+speed);
		
		MiniNumber megspeed = speed.div(MiniNumber.MILLION).setSignificantDigits(4);
		
		JSONObject resp = new JSONObject();
		resp.put("hashes", hashes);
		resp.put("millitime", timediff);
		resp.put("speed", megspeed.toString()+" MH/s");
		
		//Add balance..
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new hashtest();
	}

}
