package org.minima.system.commands.base;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;

import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMRData;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.kissvm.Contract;
import org.minima.kissvm.expressions.ConstantExpression;
import org.minima.kissvm.functions.general.FUNCTION;
import org.minima.kissvm.values.StringValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.CoinProof;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
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
		resp.put("result", res);
		if(logs) {
			resp.put("logs", ctr.getCompleteTraceLog());
		}
		
		//Add to response
		ret.put("response", resp);
				
		return ret;
	}

	@Override
	public Command getFunction() {
		return new maths();
	}

}
