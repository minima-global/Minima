package org.minima.system.commands.all;

import org.minima.system.commands.Command;
import org.minima.system.params.GeneralParams;
import org.minima.utils.json.JSONObject;

public class debugfunc extends Command {

	public debugfunc() {
		super("debugfunc","(activate:true|false) - Custom debug stuff..");
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();

		String txpowon = getParam("activate", "true");
		
		//For now - stop accepting 
		if(txpowon.equals("true")) {
			//Accept txpow messages
			GeneralParams.DEBUGFUNC = true;
		}else {
			//Don't accept txpow messages - pulse does that
			GeneralParams.DEBUGFUNC = false;
		}
		
		//Add balance..
		ret.put("response", "DEBUGFUNC set to "+GeneralParams.DEBUGFUNC);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new debugfunc();
	}

}
