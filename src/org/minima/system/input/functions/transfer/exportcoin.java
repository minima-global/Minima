package org.minima.system.input.functions.transfer;

import org.minima.objects.base.MiniData32;
import org.minima.system.brains.ConsensusUser;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class exportcoin extends CommandFunction{

	public exportcoin() {
		super("exportcoin");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//The CoinID
		String pubks = zInput[1];
		
		//Connect to a specific host:port
		Message export  = new Message(ConsensusUser.CONSENSUS_EXPORTCOIN).addObject("coinid", new MiniData32(pubks));
		getMainHandler().getConsensusHandler().PostMessage(export);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new exportcoin();
	}
}
