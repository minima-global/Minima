package org.minima.system.commands.backup.mmrsync;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.mmr.MegaMMR;
import org.minima.system.commands.Command;
import org.minima.system.params.GeneralParams;
import org.minima.utils.json.JSONObject;

public class megammr extends Command {

	public megammr() {
		super("megammr","Scan the Mega MMR");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","publickey","phrase"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();

		String action = getParam("action","info");
		
		MegaMMR megammr = MinimaDB.getDB().getMegaMMR();
		
		if(action.equals("info")) {
		
			JSONObject resp = new JSONObject();
			resp.put("enabled", GeneralParams.IS_MEGAMMR);
			resp.put("mmr", megammr.getMMR().toJSON(false));
			resp.put("coins", megammr.getAllCoins().size());
			
			//Put the details in the response..
			ret.put("response", resp);
		
		}else if(action.equals("backup")) {
			
			//Export the MegaMMR and an IBD..
			
			
		}
		
		return ret;
	}
	
	@Override
	public Command getFunction() {
		return new megammr();
	}

}
