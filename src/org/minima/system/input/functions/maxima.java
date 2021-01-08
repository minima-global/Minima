package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusUser;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class maxima extends CommandFunction{

	public maxima() {
		super("maxima");
		
		setHelp("[to:] [message:]", "Post a Maxima message.", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
//		//The details
//		String coinid = zInput[1];
//		
//		//Create a message
//		Message sender = getResponseMessage(ConsensusUser.CONSENSUS_KEEPCOIN);
//		sender.addString("coinid", coinid);
//		
//		//Send it to the miner..
//		getMainHandler().getConsensusHandler().PostMessage(sender);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new maxima();
	}
}
