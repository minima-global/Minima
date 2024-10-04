package org.minima.system.commands.base;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.Crypto;
import org.minima.utils.MiniFile;
import org.minima.utils.json.JSONObject;

public class timemilli extends Command {

	public timemilli() {
		super("timemilli","Returns the current time in milliseconds");
	}
	
	@Override
	public String getFullHelp() {
		return "\timemilli\n"
				+ "\n"
				+ "Return the current timemilli. Can go back minutes or hours.\n"
				+ "\n"
				+ "minutesback: (optional)\n"
				+ "    Go back this many minutes\n"
				+ "\n"
				+ "hoursback: (optional)\n"
				+ "    Go back this many hours\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "timemilli\n"
				+ "\n"
				+ "timemilli minutesback:120\n"
				+ "\n"
				+ "timemilli hoursback:24\n"
				+ "\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"minutesback","hoursback"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		long timenow 	= 	System.currentTimeMillis();
		
		if(existsParam("minutesback")) {
			long minback = 1000 * 60 * getNumberParam("minutesback").getAsInt();
			timenow -= minback; 
		}else if(existsParam("hoursback")) {
			long minback = 1000 * 60 * 60 * getNumberParam("hoursback").getAsInt();
			timenow -= minback; 
		}
		
		Date dd = new Date(timenow);
		
		JSONObject resp = new JSONObject();
		resp.put("timemilli", timenow);
		resp.put("date", dd.toString());
		
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new timemilli();
	}

}
