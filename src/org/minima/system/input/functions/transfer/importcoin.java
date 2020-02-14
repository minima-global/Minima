package org.minima.system.input.functions.transfer;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniHash;
import org.minima.system.brains.ConsensusUser;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class importcoin extends CommandFunction{

	public importcoin() {
		super("importcoin");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//The CoinID data
		String data = zInput[1];
		
		Message importmsg  = getResponseMessage(ConsensusUser.CONSENSUS_IMPORTCOIN)
								.addObject("proof", new MiniData(data));
		
		getMainHandler().getConsensusHandler().PostMessage(importmsg);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new importcoin();
	}
}
