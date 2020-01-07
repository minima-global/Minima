package org.minima.system.input.functions.txns;

import org.minima.system.brains.ConsensusTxn;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class txnlist extends CommandFunction {

	public txnlist() {
		super("txnlist");
		setHelp("", "List all current custom transactions", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		getMainHandler().getConsensusHandler().PostMessage(getResponseMessage(ConsensusTxn.CONSENSUS_TXNLIST));
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new txnlist();
	}
}
