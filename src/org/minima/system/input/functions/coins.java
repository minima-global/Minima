package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusPrint;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class coins extends CommandFunction {

	public coins() {
		super("coins");
		setHelp("", "(address) Either return all coins or all coins of a given address", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Get the current balance of the user for all tokens..
		Message msg = getResponseMessage(ConsensusPrint.CONSENSUS_COINS);
				
		if(zInput.length>1) {
			//Its an adddress
			msg.addString("address", zInput[1]);	
		}
			
		//Post It..
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new coins();
	}
}
