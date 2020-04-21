package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusPrint;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class coinsimple extends CommandFunction {

	public coinsimple() {
		super("coinsimple");
		setHelp("[tokenid]", "Return all your simple sendable coins of the given tokenid.", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Get the current balance of the user for all tokens..
		Message msg = getResponseMessage(ConsensusPrint.CONSENSUS_COINSIMPLE);
		msg.addString("tokenid", zInput[1]);
		
		//Post It..
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new coinsimple();
	}
}
