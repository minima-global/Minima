package org.minima.system.input.functions.transfer;

import org.minima.objects.base.MiniData;
import org.minima.system.brains.ConsensusUser;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class exportcoin extends CommandFunction{

	public exportcoin() {
		super("exportcoin");
		
		setHelp("[coinid]", "export the proof of a single coin. Share it.", 
				"Never worry about losing your coin proofs. Share the details with friends, "
				+ "they can't spend the coins, and retrieve them when you need them.");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//The CoinID
		String pubks = zInput[1];
		
		Message export  = getResponseMessage(ConsensusUser.CONSENSUS_EXPORTCOIN)
								.addObject("coinid", new MiniData(pubks));
		
		getMainHandler().getConsensusHandler().PostMessage(export);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new exportcoin();
	}
}
