package org.minima.system.commands.all;

import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMR;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONObject;

public class printmmr extends Command {

	public printmmr() {
		super("printmmr", "");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		MMR mmr = MinimaDB.getDB().getTxPoWTree().getTip().getMMR();
		
		ret.put("responsse", mmr.toJSON());
		return ret;
	}

	@Override
	public Command getFunction() {
		return new printmmr();
	}

}
