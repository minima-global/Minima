package org.minima.system.commands.base;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.wallet.KeyRow;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Address;
import org.minima.objects.base.MiniData;
import org.minima.objects.keys.TreeKey;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.BIP39;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class coinnotify extends Command {

	public coinnotify() {
		super("coinnotify","[action:] [address:] - listen for a specific coin address and send a NOTIFYCOIN message when found in chain");
	} 
	
	@Override
	public String getFullHelp() {
		return "\ncoinnotify\n"
				+ "\n"
				+ "Listen for a specific coin address - without adding it to scripts.\n"
				+ "You need to do this every startup.. from your Minidapp service.js for example\n"
				+ "\n"
				+ "action: \n"
				+ "    add : Add to the list - only added once if already added.\n"
				+ "    remove : Remove from the list\n"
				+ "    check : Check if in the list.\n"
				+ "\n"
				+ "address:\n"
				+ "    The address to look out for.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "coinnotify action:add address:0xFFEEDD..\n"
				+ "\n"
				+ "coinnotify action:remove address:0xFFEEDD..\n"
				+ "\n"
				+ "coinnotify action:check address:Mx12ABGF56..\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","address"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		//Which action
		String action = getParam("action");
		
		//Get the address
		String addr = getAddressParam("address");
		
		JSONObject resp = new JSONObject();
		resp.put("address", addr);
		
		if(action.equals("add")) {
			MinimaDB.getDB().addCoinNotify(addr);
		
		}else if(action.equals("remove")) {
			boolean found = MinimaDB.getDB().removeCoinNotify(addr);
			resp.put("found", found);
			
		}else if(action.equals("check")) {
			boolean found = MinimaDB.getDB().checkCoinNotify(addr);
			resp.put("found", found);
		
		}else {
			throw new CommandException("Invalid action : "+action);
		}
		
		//Put the details in the response..
		ret.put("response", resp);
		
		return ret;
	}
		
	@Override
	public Command getFunction() {
		return new coinnotify();
	}
}
