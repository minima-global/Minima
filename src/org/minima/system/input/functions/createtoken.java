package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusHandler;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class createtoken extends CommandFunction {
	
	public createtoken() {
		super("createtoken");
		setHelp("[name|description] [total tokens] (token script hash)", "Create a token with the given name or description, amount, and with the optional script. This currently colors upto 0.001 Minima. The TokenID generated is one time and globaly unique.", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Take the Amount..
		String name   = zInput[1];
		String amount = zInput[2];
		
		//Send to the consensus Handler
		Message msg = getResponseMessage(ConsensusHandler.CONSENSUS_CREATETOKEN);
		msg.addString("name", name);
		msg.addString("amount", amount);
	
		//Post it!
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		return new createtoken();
	}
	
}
