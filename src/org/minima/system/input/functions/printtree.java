package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusPrint;
import org.minima.system.input.CommandFunction;

public class printtree extends CommandFunction{

	public printtree() {
		super("printtree");
		setHelp("", "Print just a stripped down tree of the current chain", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Print the Tree..
		getMainHandler().getConsensusHandler().PostMessage(ConsensusPrint.CONSENSUS_PRINTCHAIN_TREE);
		
		//It's worked
		getResponseStream().endStatus(true, "");
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new printtree();
	}
}
