package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusHandler;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class createtoken extends CommandFunction {
	
	public createtoken() {
		super("createtoken");
		setHelp("[amount]", "Create a token with the given amount. The TokenID is one time and globally unique.", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Take the Amount..
		String amount = zInput[1];
		
		//Send to the consensus Handler
		Message msg = getResponseMessage(ConsensusHandler.CONSENSUS_CREATETOKEN);
		msg.addString("amount", amount);
	
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		return new createtoken();
	}
	
}
