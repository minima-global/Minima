package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusPrint;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class search extends CommandFunction{

	public search() {
		super("search");
		
		setHelp("[address] (spent|unspent)", "Search for any coins of the given address. Deafaults to UNSPENT", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//The details
		String address = zInput[1];
		
		//Create a message
		Message sender = getResponseMessage(ConsensusPrint.CONSENSUS_SEARCH);
		sender.addString("address", address);
		
		sender.addBoolean("spent", false);
		if(zInput.length>2) {
			if(zInput[2].equals("spent")) {
				sender.addBoolean("spent", true);
			}
		}
		
		//Send it to the miner..
		getMainHandler().getConsensusHandler().PostMessage(sender);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new search();
	}
}
