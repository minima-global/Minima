package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusPrint;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class txpowsearch extends CommandFunction{

	public txpowsearch() {
		super("txpowsearch");
		
		setHelp("[address]", "Search for any TXPOW messages with address as an INPUT","");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//The details
		String address = zInput[1];
		
		//Create a message
		Message sender = getResponseMessage(ConsensusPrint.CONSENSUS_TXPOWSEARCH);
		sender.addString("address", address);
		
		//Send it to the miner..
		getMainHandler().getConsensusHandler().PostMessage(sender);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new txpowsearch();
	}
}
