package org.minima.system.commands.scripts;

import org.minima.database.MinimaDB;
import org.minima.database.wallet.ScriptRow;
import org.minima.database.wallet.Wallet;
import org.minima.kissvm.Contract;
import org.minima.objects.Address;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONObject;

public class newscript extends Command {

	public newscript() {
		super("newscript","[script:] [trackall:false|true] - Add a new custom script. Track ALL addresses or just ones with relevant state variables.");
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		//Get the wallet..
		Wallet wallet = MinimaDB.getDB().getWallet();
		
		//Get the script
		String script = getParam("script");
		boolean track = getBooleanParam("trackall");
		
		//Clean the script
		script = Contract.cleanScript(script);
		
		//Do we have this script already..
		Address addr = new Address(script);
		
		//Load it...
		ScriptRow scr = wallet.getScriptFromAddress(addr.getAddressData().to0xString());
		
		//Is it's the sdasme script and the same TRACK
		if(scr!=null && (scr.isTrack()==track)) {
			
			//Put the details in the response..
			ret.put("response", scr.toJSON());
			
		}else {
		
			//Now add it to the DB
			ScriptRow krow = wallet.addScript(script, false, false, "0x00", track);
			
			//Put the details in the response..
			ret.put("response", krow.toJSON());
		}
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new newscript();
	}

}
