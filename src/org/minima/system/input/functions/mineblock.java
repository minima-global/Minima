package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusHandler;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class minesingle extends CommandFunction{

	public minesingle() {
		super("minesingle");
		
		setHelp("", "Mine 1 single block - useful for debugging", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Create a message
		Message minesone = getResponseMessage(ConsensusHandler.CONSENSUS_DEBUGMINE);
		
		//Send it to the miner..
		getMainHandler().getConsensusHandler().PostMessage(minesone);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new minesingle();
	}
}
