package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusUser;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class keepcoin extends CommandFunction{

	public keepcoin() {
		super("keepcoin");
		
		setHelp("[coinid]", "Add a coin to the list of coins you keep track of. MUST be a recent coin in the current blocks.", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//The details
		String coinid = zInput[1];
		
		//Create a message
		Message sender = getResponseMessage(ConsensusUser.CONSENSUS_KEEPCOIN);
		sender.addString("coinid", coinid);
		
		//Send it to the miner..
		getMainHandler().getConsensusHandler().PostMessage(sender);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new keepcoin();
	}
}
