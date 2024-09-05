package org.minima.system.commands.send.wallet;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.commands.CommandRunner;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class createfrom extends Command {

	public class AddressAmount {
		
		MiniData 	mAddress;
		MiniNumber 	mAmount;
		
		public AddressAmount(MiniData zAddress, MiniNumber zAmount) {
			mAddress 	= zAddress;
			mAmount		= zAmount;
		}
		
		public MiniData getAddress(){
			return mAddress;
		}
		
		public MiniNumber getAmount() {
			return mAmount;
		}
	}
	
	public createfrom() {
		super("createfrom","[fromaddress:] [address:] [amount:] (tokenid:) [script:] (burn:) - Create unsigned txn from a certain address");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"fromaddress","address",
				"amount","tokenid","script","burn"}));
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
		
		//Thew script of the address
		String script 		= getParam("script");
		
		//ID of the custom transaction
		String randomid 	= MiniData.getRandomData(32).to0xString();
		
		//Now construct the transaction..
		JSONObject result 	= runCommand("txncreate id:"+randomid);
		
		//Add the mounts..
		String command 		= "txnaddamount id:"+randomid+" burn:"+burn+" fromaddress: "+fromaddress+" address:"+toaddress+" amount:"+amount+" tokenid:"+tokenid;
		result = runCommand(command);
		if(!(boolean)result.get("status")) {
			
			//Delete transaction
			runCommand("txndelete id:"+randomid);
			
			//Not enough funds!
			throw new CommandException(result.getString("error"));
		}
		
		//Add the scripts..
		runCommand("txnscript id:"+randomid+" scripts:{\""+script+"\":\"\"}");
		
		//Sort the MMR
		runCommand("txnmmr id:"+randomid);
		
		//Now export the txn..
		result = runCommand("txnexport id:"+randomid);
		
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
		return new createfrom();
	}	
}