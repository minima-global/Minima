package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusPrint;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class search extends CommandFunction{

	public search() {
		super("search");
		
		setHelp("[address]", "Search for any unspent coins of the given address", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//The details
		String address = zInput[1];
		
		//Create a message
		Message sender = getResponseMessage(ConsensusPrint.CONSENSUS_SEARCH);
		sender.addString("address", address);
		
		//Send it to the miner..
		getMainHandler().getConsensusHandler().PostMessage(sender);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new search();
	}
}
