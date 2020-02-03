package org.minima.system.input.functions;

import java.math.BigDecimal;

import org.minima.system.brains.ConsensusHandler;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class createtokentoo extends CommandFunction {
	
	public createtokentoo() {
		super("createtokentoo");
		setHelp("[name] [total tokens] {token script hash}", "Create a token with the given name and amount, and with the optional script. This colors upto 0.1 Minima. The TokenID is one time and globally unique.", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Take the Amount..
		String name   = zInput[1];
		String amount = zInput[2];
		
		//Send to the consensus Handler
		Message msg = getResponseMessage(ConsensusHandler.CONSENSUS_CREATEFULLTOKEN);
		msg.addString("name", name);
		msg.addString("amount", amount);
	
		//Post it!
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		return new createtokentoo();
	}
}
