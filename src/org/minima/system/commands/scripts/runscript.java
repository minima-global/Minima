package org.minima.system.commands.scripts;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.mmr.MMRProof;
import org.minima.kissvm.Contract;
import org.minima.kissvm.values.StringValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.Address;
import org.minima.objects.ScriptProof;
import org.minima.objects.StateVariable;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class runscript extends Command {

	public runscript() {
		super("runscript","[script:] (state:{}) (prevstate:{}) (globals:{}) (signatures:[]) (extrascripts:{}) - Run a script with the defined parameters");
	}
	
	@Override
	public String getFullHelp() {
		return "\nrunscript\n"
				+ "\n"
				+ "Test run a script with predefined parameters without executing on chain.\n"
				+ "\n"
				+ "Scripts will be auto cleaned for you.\n"
				+ "\n"
				+ "script:\n"
				+ "    The script to run, surrounded by double quotes.\n"
				+ "\n"
				+ "state: (optional)\n"
				+ "    State variable values to use when running the script.\n"
				+ "    JSON object in the format {0:value,1:value,..}.\n"
				+ "\n"
				+ "prevstate: (optional)\n"
				+ "    The previous state variable values (for the input coin) to use when running the script.\n"
				+ "    JSON object in the format {0:value,1:value,..}.\n"
				+ "\n"
				+ "globals: (optional)\n"
				+ "    The Global variable values to use when running the script.\n"
				+ "    JSON object in the format {@GLOBAL:value,..}.\n"
				+ "\n"
				+ "signatures: (optional)\n"
				+ "    The signatures required for the script. JSON array.\n"
				+ "\n"
				+ "extrascripts: (optional)\n"
				+ "    Extra scripts required for MAST contracts. \n"
				+ "    JSON object in the format {script:proof,..}.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "runscript script:\"RETURN SIGNEDBY(0xFF..) AND @BLOCK GT 100\" globals:{\"@BLOCK\":\"101\"} signatures:[\"0xFF\"]\n"
				+ "\n"
				+ "runscript script:\"LET st=STATE(99) LET ps=PREVSTATE(99) IF st EQ ps AND @COINAGE GT 20\n"
				+ "AND SIGNEDBY(0xFF) THEN RETURN TRUE ELSEIF st GT ps AND SIGNEDBY(0xEE) THEN RETURN TRUE ENDIF\"\n"
				+ "globals:{\"@COINAGE\":\"23\"} state:{\"99\":\"0\"} prevstate:{\"99\":\"0\"} signatures:[\"0xFF\"]\n"
				+ "\n"
				+ "runscript script:\"MAST 0x0E3..\" extrascripts:{\"RETURN TRUE\":\"0x000..\"}\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"script","state","prevstate","globals","signatures","extrascreipt"}));
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
		
		JSONObject extrascripts = new JSONObject();
		if(existsParam("extrascripts")) {
			extrascripts = getJSONObjectParam("extrascripts");
		}
		
		//The Transaction..
		Transaction trans 	= new Transaction();
		Witness witness 	= new Witness();
		
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
		
		//Any extra scripts
		for(Object key : extrascripts.keySet()) {
			
			//Get the state var..
			String exscript = (String)key;
			
			//The Key is a String
			String proof 		=  (String) extrascripts.get(key);
			MiniData proofdata 	= new MiniData(proof); 
			
			//Make it into an MMRProof..
			MMRProof scproof = MMRProof.convertMiniDataVersion(proofdata);
			
			//Create a ScriptProof..
			ScriptProof scprf = new ScriptProof(exscript, scproof);
			
			//Add to the Witness..
			witness.addScript(scprf);
		}

		
		//Create a Contract
		Contract contract = new Contract(script, sigs, witness, trans, pstate);
	
		//Set trhe Script..
		contract.setGlobalVariable("@SCRIPT", new StringValue(script));
		
		//Is there enough for @COINAGE
		if(globals.containsKey("@BLOCK") && globals.containsKey("@CREATED") && !globals.containsKey("@COINAGE")) {
			MiniNumber block   		= new MiniNumber((String)globals.get("@BLOCK")); 
			MiniNumber inblock 		= new MiniNumber((String)globals.get("@CREATED"));
			MiniNumber blockdiff 	= block.sub(inblock);
			globals.put("@COINAGE", blockdiff.toString());
		}
		
		//Set the Globals..
		for(Object key : globals.keySet()) {
			
			//The Key is a String
			String glob = (String)key; 
		
			//What is the Value..
			String val = (String) globals.get(key);
			
			//And add..
			contract.setGlobalVariable(glob.toUpperCase(), Value.getValue(val));
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
		scriptnormal.put("address", new Address(script).getAddressData().to0xString());
		resp.put("script", scriptnormal);
		
		JSONObject scriptclean 	= new JSONObject();
		String cleanscript 		= Contract.cleanScript(script);
		scriptclean.put("script", cleanscript);
		scriptclean.put("address", new Address(cleanscript).getAddressData().to0xString());
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
