package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusUser;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class flushmempool extends CommandFunction{

	public flushmempool() {
		super("flushmempool");
		
		setHelp("", "Check mempool transactions and remove invalid - they may be stuck..", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Create a message
		Message sender = getResponseMessage(ConsensusUser.CONSENSUS_FLUSHMEMPOOL);
		
		//Send it to the miner..
		getMainHandler().getConsensusHandler().PostMessage(sender);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new flushmempool();
	}
}
