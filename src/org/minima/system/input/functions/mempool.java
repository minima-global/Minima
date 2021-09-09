package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusUser;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class mempool extends CommandFunction{

	public mempool() {
		super("mempool");
		
		setHelp("", "Show details about mempool transactions..", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Create a message
		Message sender = getResponseMessage(ConsensusUser.CONSENSUS_MEMPOOL);
		
		//Send it to the miner..
		getMainHandler().getConsensusHandler().PostMessage(sender);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new mempool();
	}
}
