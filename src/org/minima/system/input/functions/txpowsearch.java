package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusPrint;
import org.minima.system.brains.ConsensusUser;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class txpowsearch extends CommandFunction{

	public txpowsearch() {
		super("txpowsearch");
		
		setHelp("(input:address) (output:address) (tokenid:tokenid)", "Search for TXPOW messages with address as an input or output or any tokenid","");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		int len = zInput.length;
		if(len == 1) {
			getResponseStream().endStatus(false, "MUST specify some criteria for search..");
			return;
		}
		
		//The extra data
		String input        = "";
		String output       = "";
		String token        = "";
		
		//Cycle through..
		for(int i=1;i<len;i++) {
			String param = zInput[i];
			
			if(param.startsWith("input:")) {
				input = param.substring(6);
			}else if(param.startsWith("output:")) {
				output = param.substring(7);
			}else if(param.startsWith("tokenid:")) {
				token = param.substring(8);
			}
		}
		
		//Create the message
		Message sender = getResponseMessage(ConsensusPrint.CONSENSUS_TXPOWSEARCH);
		sender.addString("input", input);
		sender.addString("output", output);
		sender.addString("tokenid", token);
		
		//Send it to the miner..
		getMainHandler().getConsensusHandler().PostMessage(sender);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new txpowsearch();
	}
}
