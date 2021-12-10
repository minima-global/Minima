package org.minima.system.commands.all;

import java.util.ArrayList;

import org.minima.kissvm.Contract;
import org.minima.kissvm.values.StringValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.Address;
import org.minima.objects.StateVariable;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class runscript extends Command {

	public runscript() {
		super("runscript","[script:] (state:{}) (prevstate:{}) (globals:{}) (signatures:[])- Run a script with the defined parameters");
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();

		//Get the script..
		String script = getParam("script","");
		
		//What is defined..
		JSONObject state = new JSONObject();
		if(existsParam("state")) {
			state = getJSONObjectParam("state");
		}
		
		JSONObject prevstate = new JSONObject();
		if(existsParam("prevstate")) {
			prevstate = getJSONObjectParam("prevstate");
		}
		
		JSONObject globals = new JSONObject();
		if(existsParam("globals")) {
			globals = getJSONObjectParam("globals");
		}
		
		JSONArray signatures = new JSONArray();
		if(existsParam("signatures")) {
			signatures = getJSONArrayParam("signatures");
		}
		
		//The Transaction..
		Transaction trans = new Transaction();
				
		//Add the state variables..
		for(Object key : state.keySet()) {
			
			//The Key is a String
			String portstr = (String)key; 
			
			//The port
			int port = Integer.parseInt(portstr);
			
			//Get the state var..
			String var = (String) state.get(key);

			//Create a state variable..
			StateVariable sv = new StateVariable(port, var);
			
			//Add to the transaction..
			trans.addStateVariable(sv);
		}
		
		//Add the Previous State variables
		ArrayList<StateVariable> pstate = new ArrayList<>();
		for(Object key : prevstate.keySet()) {
			
			//The Key is a String
			String portstr = (String)key; 
			
			//The port
			int port = Integer.parseInt(portstr);
			
			//Get the state var..
			String var = (String) prevstate.get(key);

			//Create a state variable..
			StateVariable sv = new StateVariable(port, var);
			
			//Add to the transaction..
			pstate.add(sv);
		}
	
		//Add the Signatures
		ArrayList<MiniData> sigs = new ArrayList<>();
		for(Object sig : signatures) {
			
			//The String sig
			String strsig = (String)sig;
			
			//Add them to our list
			sigs.add(new MiniData(strsig));
		}
		
		//Create a Contract
		Contract contract = new Contract(script, sigs, new Witness(), trans, pstate);
	
		//Set trhe Script..
		contract.setGlobalVariable("@SCRIPT", new StringValue(script));
		
		//Set the Globals..
		for(Object key : globals.keySet()) {
			
			//The Key is a String
			String glob = (String)key; 
		
			//What is the Value..
			String val = (String) globals.get(key);
			
			//And add..
			contract.setGlobalVariable(glob, Value.getValue(val));
		}
		
		//Run it
		contract.run();
		
		boolean parse 		= contract.isParseOK();
		boolean monotonic 	= contract.isMonotonic();
		boolean success 	= contract.isSuccess();
		
		//The Response..
		JSONObject resp = new JSONObject();
		
		JSONObject scriptnormal = new JSONObject();
		scriptnormal.put("script", script);
		scriptnormal.put("address", new Address(script).getAddressData());
		resp.put("script", scriptnormal);
		
		JSONObject scriptclean 	= new JSONObject();
		String cleanscript 		= Contract.cleanScript(script);
		scriptclean.put("script", cleanscript);
		scriptclean.put("address", new Address(cleanscript).getAddressData());
		resp.put("clean", scriptclean);
		
		resp.put("trace", contract.getCompleteTraceLog());
		
		resp.put("parseok", parse);
		resp.put("monotonic", monotonic);
		resp.put("success", success);
		
		//Add balance..
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new runscript();
	}

}
