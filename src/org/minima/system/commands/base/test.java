package org.minima.system.commands.base;

import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;

public class test extends Command {

	public test() {
		super("test","test Funxtion");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		MinimaLogger.log("Restarting NIO..");
		
		//Restart the NIO..
		Main.getInstance().restartNIO();
		
		MinimaLogger.log("Done..");
		
		ret.put("response", true);
		
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new test();
	}

}
