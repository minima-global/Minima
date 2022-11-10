package org.minima.system.commands.base;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.userprefs.UserDB;
import org.minima.objects.base.MiniNumber;
import org.minima.system.commands.Command;
import org.minima.system.params.GeneralParams;
import org.minima.utils.json.JSONObject;

public class magic extends Command {

	public magic() {
		super("magic","");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"kissvm"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();

		JSONObject resp = new JSONObject();
		
		UserDB udb = MinimaDB.getDB().getUserDB();
		
		
		if(existsParam("kissvm")) {
			
			MiniNumber num = getNumberParam("kissvm");
			
			//Set this as your KISSVM opcodes..
			udb.setMagicDesiredKISSVM(num);
		}
		
		
		resp.put("desired_kissvm", udb.getMagicDesiredKISSVM());
		
		//Add balance..
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new magic();
	}

}
