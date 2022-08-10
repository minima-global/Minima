package org.minima.system.commands.base;

import java.util.ArrayList;

import org.minima.objects.IBD;
import org.minima.objects.TxBlock;
import org.minima.objects.TxPoW;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.network.maxima.MaximaManager;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

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
