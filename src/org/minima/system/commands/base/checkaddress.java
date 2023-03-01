package org.minima.system.commands.base;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Address;
import org.minima.objects.base.MiniData;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.json.JSONObject;

public class checkaddress extends Command {

	public checkaddress() {
		super("checkaddress","[address:] - Check an address is valid");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"address"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();

		//First get the address
		String address = getAddressParam("address");

		if(address.startsWith("0x") && address.length() != 66) {
			//Hmm. should be 66 chars long..
			throw new CommandException("Invalid Length for 0x address should be 66 chars long : "+address.length());
		}
		
		if(!address.startsWith("Mx") && !address.startsWith("0x")) {
			throw new CommandException("Address does not start with 0x or Mx");
		}
		
		//Check if this is one of out address
		Wallet wallet = MinimaDB.getDB().getWallet();
		
		
		MiniData data 	= new MiniData(address);
		String datastr 	= data.to0xString(); 
		
		JSONObject res = new JSONObject();
		res.put("original", address);
		res.put("0x", datastr);
		res.put("Mx", Address.makeMinimaAddress(data));
		res.put("relevant", wallet.isAddressRelevant(datastr));
		res.put("simple", wallet.isAddressSimple(datastr));
		
		ret.put("response", res);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new checkaddress();
	}

}
