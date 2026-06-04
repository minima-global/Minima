package org.minima.system.commands.send.wallet;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.objects.StateVariable;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.commands.CommandRunner;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class sendfrom extends Command {
	
	public sendfrom() {
		super("sendfrom","[fromaddress:] [address:] [amount:] [script:] [privatekey:] [keyuses:] (tokenid:) (state:) (split:) (burn:) (mine:) - Send Minima or Tokens from a certain address");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"fromaddress","address",
				"amount","tokenid","script","privatekey","keyuses","mine","burn","state","split"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
	
		//From which address
		String fromaddress 	= getAddressParam("fromaddress");
		String toaddress 	= getAddressParam("address");
		MiniNumber amount 	= getNumberParam("amount");
		String tokenid 		= getAddressParam("tokenid", "0x00");
		
		//Get the BURN
		MiniNumber burn 	= getNumberParam("burn",MiniNumber.ZERO);
		if(burn.isMore(MiniNumber.ZERO) && !tokenid.equals("0x00")) {
			throw new CommandException("Currently BURN on precreated transactions only works for Minima.. tokenid:0x00.. not tokens.");
		}
		
		//The script of the address
		String script 		= getParam("script");
		
		//The private key we need to sign with
		String privatekey	= getAddressParam("privatekey");
		MiniNumber keyuses  = getNumberParam("keyuses");
		
		//ID of the custom transaction
		String randomid 	= MiniData.getRandomData(32).to0xString();
		
		//Are we mining
		boolean mine 		= getBooleanParam("mine", true);
		
		//Is there a split output
		MiniNumber split = getNumberParam("split", MiniNumber.ONE);
				
		//Is there a state
		JSONObject state = null;
		if(existsParam("state")) {
			try {
				state = getJSONObjectParam("state");
			}catch(Exception exc) {
				state = null;
			}
		}
		
		//Now construct the transaction..
		JSONObject result 	= runCommand("txncreate id:"+randomid);
		
		//Add the mounts..
		String command 		= "txnaddamount id:"+randomid+" split:"+split+" burn:"+burn
				+" fromaddress: "+fromaddress+" address:"+toaddress+" amount:"+amount+" tokenid:"+tokenid;
		result = runCommand(command);
		if(!(boolean)result.get("status")) {
			
			//Delete transaction
			runCommand("txndelete id:"+randomid);
			
			//Not enough funds!
			throw new CommandException(result.getString("error"));
		}
		
		//Add the scripts..
		runCommand("txnscript id:"+randomid+" scripts:{\""+script+"\":\"\"}");
		
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
		
		//Sort the MMR
		runCommand("txnmmr id:"+randomid);
		
		//Now SIGN
		runCommand("txnsign id:"+randomid+" publickey:custom privatekey:"+privatekey+" keyuses:"+keyuses);
		
		//And POST!
		result = runCommand("txnpost id:"+randomid+" mine:"+mine);
		
		//And delete..
		runCommand("txndelete id:"+randomid);
		
		//Check succeeded..
		if(!(boolean)result.get("status")) {
			//Didn't work..
			throw new CommandException((String)result.get("error"));
		}
		
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
		return new sendfrom();
	}	
}