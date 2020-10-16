package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusPrint;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class status extends CommandFunction {

	public status() {
		super("status");
		
		setHelp("(full)","Show the status of the Minima network. Full also calculates the IBD and TxPOW folder size.. ", 
				"A complete view of the current state of the Minima network");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		Message status = getResponseMessage(ConsensusPrint.CONSENSUS_STATUS);
		
		//Are we full..
		if(zInput.length>1) {
			status.addBoolean("full", true);
		}else {
			status.addBoolean("full", false);
		}
		
		//Must be run from the thread safe Consensus thread - to access the database
		getMainHandler().getConsensusHandler().PostMessage(status);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new status();
	}
}
