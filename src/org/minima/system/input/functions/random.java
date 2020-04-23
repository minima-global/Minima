package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusPrint;
import org.minima.system.brains.ConsensusUser;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class random extends CommandFunction{

	public random() {
		super("random");
		
		setHelp("(length)", "Return a random HEX value of the length specified or 64 by default", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//The details
		int len = 64;
		if(zInput.length>1) {
			len = Integer.parseInt(zInput[1]);
		}
		
		//Create a message
		Message sender = getResponseMessage(ConsensusPrint.CONSENSUS_RANDOM);
		sender.addInt("length", len);
		
		//Send it to the miner..
		getMainHandler().getConsensusHandler().PostMessage(sender);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new random();
	}
}
