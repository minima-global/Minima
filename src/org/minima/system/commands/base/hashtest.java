package org.minima.system.commands.base;

import org.minima.objects.base.MiniData;
import org.minima.system.commands.Command;
import org.minima.utils.Crypto;
import org.minima.utils.json.JSONObject;

public class hashtest extends Command {

	public hashtest() {
		super("hashtest","(amount:) - Check the speed of hashing of this device. Defaults to 1 million hashes");
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();

		//How many hashes to perform
		int hashes = Integer.parseInt(getParam("amount", "1000000"));
		
		long timestart = System.currentTimeMillis();
		
		MiniData data = MiniData.getRandomData(32);
		for(int i=0;i<hashes;i++) {
			data = Crypto.getInstance().hashObject(data);
		}
		
		long timediff = System.currentTimeMillis() - timestart;
		
		float speed 	= ( hashes * 1000 ) / timediff; 
		float megspeed 	= speed / 1000000;
		
		String spd = String.format("%.3f MHash/s",megspeed);
		
		JSONObject resp = new JSONObject();
		resp.put("hashes", hashes);
		resp.put("time", timediff);
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
