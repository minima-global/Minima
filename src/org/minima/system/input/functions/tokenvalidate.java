package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusPrint;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class tokenvalidate extends CommandFunction {
	
	public tokenvalidate() {
		super("tokenvalidate");
		setHelp("[tokenid]", 
				"Check that a token proof is valid", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		String tokenid = zInput[1]; 
		
		//Send to the consensus Handler
		Message msg = getResponseMessage(ConsensusPrint.CONSENSUS_TOKENVALIDATE);
		msg.addString("tokenid", tokenid);
		
		//Post it!
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		return new tokenvalidate();
	}
	
}
