package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusPrint;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class balance extends CommandFunction {

	public balance() {
		super("balance");
		
		setHelp("(address)", "Return the global Minima balance for all coins or just a specific address", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Get the current balance of the user for all tokens..
		Message msg = getResponseMessage(ConsensusPrint.CONSENSUS_BALANCE);
		
		//Can specify to check ONLY a single address..
		if(zInput.length>1) {
			msg.addString("address", zInput[1]);
		}
			
		//Post It..
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new balance();
	}
}
