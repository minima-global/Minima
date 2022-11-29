package org.minima.system.commands.scripts;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.wallet.ScriptRow;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Address;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class scripts extends Command {

	public scripts() {
		super("scripts","(address:) - Search scripts / addresses");
	}
	
	@Override
	public String getFullHelp() {
		return "\nscripts\n"
				+ "\n"
				+ "List all scripts or search for a script / basic address your node is tracking.\n"
				+ "\n"
				+ "address: (optional)\n"
				+ "    Script address or basic address to search for. Can be 0x or Mx address.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "scripts\n"
				+ "\n"
				+ "scripts address:0xFED5..\n"
				+ "\n"
				+ "scripts address:MxG087..n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"address"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		//Get the wallet..
		Wallet wallet = MinimaDB.getDB().getWallet();
		
		//Is there an address
		String address = getParam("address","");
		if(address.toLowerCase().startsWith("mx")) {
			//Convert back to normal hex..
			try {
				address = Address.convertMinimaAddress(address).to0xString();
			}catch(IllegalArgumentException exc) {
				throw new CommandException(exc.toString());
			}
		}
		
		if(address.equals("")) {
			
			//Get all the custom scripts
			ArrayList<ScriptRow> allscripts = wallet.getAllAddresses();
			
			JSONArray arr = new JSONArray();
			for(ScriptRow kr : allscripts) {
				arr.add(kr.toJSON());
			}
				
			//Put the details in the response..
			ret.put("response", arr);
			
		}else {
			
			//Search for that address
			ScriptRow scrow = wallet.getScriptFromAddress(address);
			if(scrow == null) {
				throw new CommandException("Script with that address not found");
			}
			
			//Put the details in the response..
			ret.put("response", scrow.toJSON());
		}
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new scripts();
	}

}
