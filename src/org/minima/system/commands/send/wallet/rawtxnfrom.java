package org.minima.system.commands.send.wallet;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.objects.StateVariable;
import org.minima.objects.base.MiniData;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandRunner;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class rawtxnfrom extends Command {
	
	public rawtxnfrom() {
		super("rawtxnfrom","[inputs:] [outputs:] [scripts:] (state:) - Create an unsigned transaction from a set of inputs, outputs, scripts and state");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"inputs","outputs","scripts","state"}));
	}
	
	@Override
	public String getFullHelp() {
		return "\nrawtxnfrom\n"
				+ "\n"
				+ "Create an unsigned transaction with a list of input and output JSON coins.\n"
				+ "\n"
				+ "inputs:\n"
				+ "    A JSONArray of coinids.\n"
				+ "\n"
				+ "outputs:\n"
				+ "    A JSONArray of output JSON coins with an address, amount, optional tokenid and storestate.\n"
				+ "\n"
				+ "scripts:\n"
				+ "    A JSONArray of scripts for all the input coins.\n"
				+ "\n"
				+ "state: (optional)\n"
				+ "    The JSON state.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "rawfrom inputs:[{\"coinid\":\"0x859EC55D1DC0DD66E72C1D8B6CDBD5E12D0EC5CB56255F629BAEC590047C22C4\",\"script\":\"RETURN SIGNEDBY(0x677228489AD4C14AC0D04F9BA054974A2B1D60C5A8E8D710CF9F2BF64D1EE81E)\"}] outputs:[{\"address\":\"0x6A3A060CE9D8E876E9B12DBE5D8F6A6864183BA1A63EA8CBCC14D668B5BECACF\",\"amount\":\"9.9996\",\"storestate\":false,\"tokenid\":\"0x00\"}] state:{\"0\":\"98\",\"1\":\"[MESSAGE]\"}\n"
				;
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
	
		//Get a list of the input coinID
		JSONArray inputs = getJSONArrayParam("inputs");
		
		//Get a list of output coins
		JSONArray outputs = getJSONArrayParam("outputs");
		
		//Get a list of scripts
		JSONArray scripts = getJSONArrayParam("scripts");
				
		//Is there a state
		JSONObject state = null;
		if(existsParam("state")) {
			state = getJSONObjectParam("state");
		}
		
		//ID of the custom transaction
		String randomid 	= MiniData.getRandomData(32).to0xString();
				
		//Now construct the transaction..
		JSONObject result 	= runCommand("txncreate id:"+randomid);
		
		//Add the Inputs - list of coinid
		for(Object input : inputs) {
			String coinid = (String)input;
			
			//Now add this coin..
			runCommand("txninput id:"+randomid+" coinid:"+coinid);
		}
		
		//Add the outputs
		for(Object output : outputs) {
			JSONObject out = (JSONObject)output;
			
			String address = out.getString("address");
			String amount  = out.getString("amount");
			
			//Is there a tokenid
			String tokenid = "0x00";
			if(out.containsKey("tokenid")) {
				tokenid = out.getString("tokenid");
			}
			
			//Is there a storestate
			boolean storestate = true;
			if(out.containsKey("storestate")) {
				storestate = out.getBoolean("storestate");
			}
			
			//Now add this coin..
			runCommand("txnoutput id:"+randomid+" address:"+address+" amount:"+amount+" tokenid:"+tokenid+" storestate:"+storestate);
		}
		
		//Add the Scripts
		for(Object script : scripts) {
			String scr = (String)script;
			
			//And add the script
			runCommand("txnscript id:"+randomid+" scripts:{\""+scr+"\":\"\"}");
		}
		
		//Add the state is exists
		if(state != null) {
			
			//Cycle..
			for(Object key : state.keySet()) {
				
				//The Key is a String
				String portstr = (String)key; 
				
				//The port
				int port = Integer.parseInt(portstr);
				
				//Get the state var..
				String var = state.get(key)+"";

				//Create a state variable..
				StateVariable sv = new StateVariable(port, var);
				
				//Add to the transaction..
				runCommand("txnstate id:"+randomid+" port:"+sv.getPort()+" value:\""+sv.getData()+"\"");
			}
		}
		
		//Now export the txn..
		result = runCommand("txnexport id:"+randomid+" showtxn:true");
				
		//And delete..
		runCommand("txndelete id:"+randomid);
		
		//And return..
		ret.put("response", result.get("response"));
		
		return ret;
	}
	
	public JSONObject runCommand(String zCommand) {
		JSONArray res 		= CommandRunner.getRunner().runMultiCommand(zCommand);
		JSONObject result 	= (JSONObject) res.get(0);
		return result;
	}

	@Override
	public Command getFunction() {
		return new rawtxnfrom();
	}	
}