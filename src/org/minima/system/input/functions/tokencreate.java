package org.minima.system.input.functions;

import org.minima.objects.base.MiniNumber;
import org.minima.system.brains.ConsensusHandler;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class tokencreate extends CommandFunction {
	
	public static MiniNumber MAX_COINS = new MiniNumber("1000000000");
	
	public tokencreate() {
		super("tokencreate");
		setHelp("[name:..] [amount:total_amount] (description:..) (script:..) (icon:URL) (proof:URL) ", 
				"Create a token.", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		int len = zInput.length;
		
		//The extra data
		String name         = "";
		String description  = "";
		String icon         = "";
		String proof        = "";
		String script       = "RETURN TRUE";
		String total        = "";
		
		//Cycle through..
		for(int i=1;i<len;i++) {
			String param = zInput[i];
			
			if(param.startsWith("name:")) {
				name = param.substring(5).trim();
			}else if(param.startsWith("description:")) {
				description = param.substring(12).trim();
			}else if(param.startsWith("icon:")) {
				icon = param.substring(5).trim();
			}else if(param.startsWith("proof:")) {
				proof = param.substring(6).trim();
			}else if(param.startsWith("script:")) {
				script = param.substring(7).trim();
			}else if(param.startsWith("amount:")) {
				total = param.substring(7).trim();
			}
		}
		
		//Get rid of the quotes..
		name = name.replaceAll("\"", "");
		description = description.replaceAll("\"", "");
		
		//Run some checks
		if(name.equals("")) {
			getResponseStream().endStatus(false, "MUST specify the token name");
			return;
		}
		
		if(total.equals("")) {
			getResponseStream().endStatus(false, "MUST specify the token amount");
			return;
		}
		
		MiniNumber coins = new MiniNumber(total);
		if(coins.isMore(MAX_COINS)) {
			getResponseStream().endStatus(false, MAX_COINS+" MAX.. for now..");
			return;
		}
		
		//Send to the consensus Handler
		Message msg = getResponseMessage(ConsensusHandler.CONSENSUS_TOKENCREATE);
		msg.addString("name", name);
		msg.addString("description", description);
		msg.addString("icon", icon);
		msg.addString("proof", proof);
		msg.addString("amount", total);
		msg.addString("script", script);
		
		//Post it!
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		return new tokencreate();
	}
	
}
