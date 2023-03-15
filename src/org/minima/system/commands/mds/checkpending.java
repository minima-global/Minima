package org.minima.system.commands.mds;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.mds.pending.PendingCommand;
import org.minima.utils.json.JSONObject;

public class checkpending extends Command {

	public checkpending() {
		super("checkpending","Check status of a pending command");
	}
	
	@Override
	public String getFullHelp() {
		return  "checkpending\n"
				+ "\n"
				+ "Show if a pending transaction UID is in the pending list\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "checkpending uid:0xFF..\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"uid"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		String uid = getParam("uid");
		
		boolean found = false;
		ArrayList<PendingCommand> allpending = Main.getInstance().getMDSManager().getAllPending();
		for(PendingCommand pc : allpending) {
			if(pc.getUID().equals(uid)) {
				found = true;
				break;
			}
		}
		
		JSONObject resp = new JSONObject();
		resp.put("pending", found);
		
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new checkpending();
	}

}
