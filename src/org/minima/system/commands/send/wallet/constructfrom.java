package org.minima.system.commands.send.wallet;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.StringTokenizer;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.commands.CommandRunner;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class constructfrom extends Command {

	public constructfrom() {
		super("constructfrom","[coinlist:] [script:] [toaddress:] [toamount:] [changeaddress:] [changeamount:] (tokenid:) - Create unsigned txn from a list of coins");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"coinlist","script","toaddress",
				"toamount","changeaddress","changeamount","tokenid"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
	
		//From which address
		String coinlist 	= getParam("coinlist");
		String script 		= getParam("script");
		
		String toaddress 		= getAddressParam("toaddress");
		MiniNumber toamount 	= getNumberParam("toamount");
		
		MiniNumber changeamount = getNumberParam("changeamount");
		String changeaddress 	= getAddressParam("changeaddress");
		
		//Could be a token..
		String tokenid 			= getParam("tokenid", "0x00");
		
		//ID of the custom transaction
		String randomid 	= MiniData.getRandomData(32).to0xString();
		
		JSONObject result 	= null;
		
		//Now construct the transaction..
		result 	= runCommand("txncreate id:"+randomid);
		
		//Add all the input coins
		StringTokenizer strtok = new StringTokenizer(coinlist,",");
		while(strtok.hasMoreTokens()) {
			String cc = strtok.nextToken().trim();
			
			//Add the mounts..
			String command 		= "txninput id:"+randomid+" coinid:"+cc;
			result = runCommand(command);
			if(!(boolean)result.get("status")) {
				
				//Delete transaction
				runCommand("txndelete id:"+randomid);
				
				//Not enough funds!
				throw new CommandException(result.getString("error"));
			}
		}
		
		//Add the output coin..
		result 	= runCommand("txnoutput id:"+randomid+" address:"+toaddress+" amount:"+toamount+" tokenid:"+tokenid);
		
		//Add the change
		if(changeamount.isMore(MiniNumber.ZERO)) {
			result 	= runCommand("txnoutput id:"+randomid+" address:"+changeaddress+" amount:"+changeamount+" tokenid:"+tokenid);
		}
		
		//Add the scripts..
		runCommand("txnscript id:"+randomid+" scripts:{\""+script+"\":\"\"}");
		
		//Sort the MMR
		runCommand("txnmmr id:"+randomid);
		
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
		return new constructfrom();
	}	
}