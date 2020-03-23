package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusPrint;
import org.minima.system.brains.ConsensusUser;
import org.minima.system.input.CommandFunction;

public class scripts extends CommandFunction {

	public scripts() {
		super("scripts");
		
		setHelp("", "List all your scripts & addresses", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Post It..
		getMainHandler().getConsensusHandler().PostMessage(getResponseMessage(ConsensusPrint.CONSENSUS_ADDRESSES));
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new scripts();
	}
}
