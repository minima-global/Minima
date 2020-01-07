package org.minima.system.input.functions.txns;

import org.minima.system.brains.ConsensusTxn;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class txncreate extends CommandFunction {

	public txncreate() {
		super("txncreate");
		setHelp("", "Create a new custom transaction", "");
	}

	@Override
	public void doFunction(String[] zInput) throws Exception {
		getMainHandler().getConsensusHandler().PostMessage(getResponseMessage(ConsensusTxn.CONSENSUS_TXNCREATE));
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new txncreate();
	}
}
