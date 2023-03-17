package org.minima.system.commands.base;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.kissvm.Contract;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONObject;

public class maths extends Command {

	public maths() {
		super("maths","[calculate:] (logs:)- Run maths with Minima precision MiniNumber");
	}
	
	@Override
	public String getFullHelp() {
		return "\nmaths\n"
				+ "\n"
				+ "Run some maths with Minima MiniNUmber precision\n"
				+ "\n"
				+ "Returns the result - na d full logs if required.\n"
				+ "\n"
				+ "calculate:\n"
				+ "    The maths you want calculated\n"
				+ "\n"
				+ "logs: (boolean)\n"
				+ "    true or false if you want the logs.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "maths calculate:\"1+2 * (3/4)\"\n"
				+ "\n"
				+ "maths calculate:\"1+2 * SIGDIG(1 3/4)\" logs:true\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"calculate","logs"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		String calculate = getParam("calculate");
		boolean logs 	 = getBooleanParam("logs",false);
		
		//Create the script..
		String function = "LET returnvalue = "+calculate;
		
		//Create a contract
		Contract ctr = new Contract(function, new ArrayList<>(), new Witness(), new Transaction(), new ArrayList<>(),false);
		ctr.run();
		
		//Get the value..
		String res = ctr.getVariable("returnvalue").toString();
		
		JSONObject resp = new JSONObject();
		if(logs) {
			resp.put("logs", ctr.getCompleteTraceLog());
		}
		resp.put("result", res);
		
		//Add to response
		ret.put("response", resp);
				
		return ret;
	}

	@Override
	public Command getFunction() {
		return new maths();
	}

}
