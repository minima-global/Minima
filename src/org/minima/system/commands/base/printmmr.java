package org.minima.system.commands.base;

import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMR;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONObject;

public class printmmr extends Command {

	public printmmr() {
		super("printmmr", "Print the MMR set of the tip block");
	}
	
	@Override
	public String getFullHelp() {
		return "\nprintmmr\n"
				+ "\n"
				+ "Print the MMR set of the tip block and the total number of entries in the MMR.\n"
				+ "\n"
				+ "Returns the tip block number, latest entrynumber and latest set of MMR entries.\n"
				+ ""
				+ "For each entry, details of its row, entry number, data and value of all new and updated MMR entries for the tip block.\n"
				+ "\n"
				+ "Row 1 represents the leaf nodes, entry 0 represents the first entry on a row.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "printmmr\n";
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		MMR mmr = MinimaDB.getDB().getTxPoWTree().getTip().getMMR();
		
		ret.put("response", mmr.toJSON());
		return ret;
	}

	@Override
	public Command getFunction() {
		return new printmmr();
	}

}
