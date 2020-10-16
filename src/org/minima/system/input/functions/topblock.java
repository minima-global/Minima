package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusPrint;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class topblock extends CommandFunction {

	public topblock() {
		super("topblock");
		
		setHelp("", "Return the current top txpow block", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Get the current balance of the user for all tokens..
		Message msg = getResponseMessage(ConsensusPrint.CONSENSUS_TOPBLOCK);
		
		//Post It..
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new topblock();
	}
}
