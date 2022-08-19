package org.minima.system.commands.base;

import org.minima.objects.IBD;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONObject;

public class test extends Command {

	public test() {
		super("test","test Funxtion");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		//Checking IBD pruning
		IBD current =new IBD();
		current.createCompleteIBD();
		
		IBD.printIBD(current);
		
		String chop = getParam("chop", "");
		
		if(!chop.equals("")) {
			
			//Chop the IBD..
			IBD mini = IBD.createShortenedIBD(current, chop);
		
			System.out.println("CHOPPED! @ "+chop);
			
			IBD.printIBD(mini);
		}
		
		ret.put("response", "Test run..");
		
		return ret;
	}
	
	@Override
	public Command getFunction() {
		return new test();
	}

}
