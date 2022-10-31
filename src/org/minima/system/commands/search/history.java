package org.minima.system.commands.search;

import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.objects.TxPoW;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class history extends Command {

	public history() {
		super("history","Search for all relevant TxPoW");
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
			
		ArrayList<TxPoW> txps = MinimaDB.getDB().getTxPoWDB().getSQLDB().getAllRelevant();
		
		JSONArray txns = new JSONArray();
		for(TxPoW txp : txps) {
			txns.add(txp.toJSON());
		}
	
		JSONObject resp = new JSONObject();
		resp.put("txpows", txns);
		resp.put("size", txns.size());
		
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new history();
	}

}
