package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusPrint;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class printtree extends CommandFunction{

	public printtree() {
		super("printtree");
		setHelp("", "Print just a stripped down tree of the current chain", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		Message msg = getResponseMessage(ConsensusPrint.CONSENSUS_PRINTCHAIN_TREE);
		
		//Print the Tree..
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new printtree();
	}
}
