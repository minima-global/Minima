package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusUser;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class unkeepcoin extends CommandFunction{

	public unkeepcoin() {
		super("unkeepcoin");
		
		setHelp("[coinid]", "Remove a coin form your list of tracked coins..", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//The details
		String coinid = zInput[1];
		
		//Create a message
		Message sender = getResponseMessage(ConsensusUser.CONSENSUS_UNKEEPCOIN);
		sender.addString("coinid", coinid);
		
		//Send it to the miner..
		getMainHandler().getConsensusHandler().PostMessage(sender);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new unkeepcoin();
	}
}
