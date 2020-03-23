package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusHandler;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class createtoken extends CommandFunction {
	
	public createtoken() {
		super("createtoken");
		setHelp("[name] [total] (token script hash)", 
				"Create a token.", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Take the Amount..
		String name   = zInput[1];
		String amount = zInput[2];
		String script = "RETURN TRUE";
		
		if(zInput.length>3) {
			script = zInput[3];
		}
		
		//Send to the consensus Handler
		Message msg = getResponseMessage(ConsensusHandler.CONSENSUS_CREATETOKEN);
		msg.addString("name", name);
		msg.addString("amount", amount);
		msg.addString("script", script);
		
		//Post it!
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		return new createtoken();
	}
	
}
