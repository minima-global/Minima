package org.minima.system.commands.persistent;

import org.minima.database.MinimaDB;
import org.minima.database.minidapps.MiniDAPPDB;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONObject;

public class sql extends Command {

	public sql() {
		super("sql","[sql:..] - Run SQL on a shared Database");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		//Get the SQL
		String sql = getParam("sql");
		
		//Get the MiniDAPP DB
		MiniDAPPDB db = MinimaDB.getDB().getMiniDAPPDB();
		
		//Run the SQL..
		JSONObject results = db.executeSQL(sql);
		
		//Add them..
		ret.put("response", results);
		
				
		return ret;
	}

	@Override
	public Command getFunction() {
		return new sql();
	}

}
