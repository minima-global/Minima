package org.minima.system.commands.mds;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.minidapps.MiniDAPP;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.mds.pending.PendingCommand;
import org.minima.utils.json.JSONObject;

public class checkmode extends Command {

	public checkmode() {
		super("checkmode","Check status of a pending command");
	}
	
	@Override
	public String getFullHelp() {
		return  "checkmode\n"
				+ "\n"
				+ "Show if a MiniDAPP is READ or WRITE mode\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "checkmode\n";
	}
	
//	@Override
//	public ArrayList<String> getValidParams(){
//		return new ArrayList<>(Arrays.asList(new String[]{"uid"}));
//	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		//Who called it
		String minidappid = getMiniDAPPID();
		
		JSONObject resp = new JSONObject();
		if(minidappid.equals("0x00")) {
			resp.put("name", "MINIMA");
			resp.put("mode", "WRITE");
		}else {
			//Get that MiniDAPP..
			MiniDAPP md = MinimaDB.getDB().getMDSDB().getMiniDAPP(minidappid);
			
			//Return the result
			resp.put("name", md.getName());
			resp.put("mode", md.getPermission().toUpperCase());
		}
		
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new checkmode();
	}

}
