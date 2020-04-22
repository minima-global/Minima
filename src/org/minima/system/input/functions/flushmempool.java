package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusUser;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class flushmempool extends CommandFunction{

	public flushmempool() {
		super("flushmempool");
		
		setHelp("(hard)", "Check mempool transactions and remove invalid - they may be stuck. HARD removes all.", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Create a message
		Message sender = getResponseMessage(ConsensusUser.CONSENSUS_FLUSHMEMPOOL);
		
		boolean hard = false;
		if(zInput.length>1) {
			hard = true;
		}
		
		//HARD reset simply remove them all..
		sender.addBoolean("hard", hard);
		
		//Send it to the miner..
		getMainHandler().getConsensusHandler().PostMessage(sender);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new flushmempool();
	}
}
