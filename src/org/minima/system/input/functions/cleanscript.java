package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusUser;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class cleanscript extends CommandFunction{

	public cleanscript() {
		super("cleanscript");
		
		setHelp("[script]", "Clean the script to make it a valid Minima script","");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//get the Contract
		String script = zInput[1];
		
		//Create a message
		Message rscript = getResponseMessage(ConsensusUser.CONSENSUS_CLEANSCRIPT);
		rscript.addString("script", script);
		
		getMainHandler().getConsensusHandler().PostMessage(rscript);
	}
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new cleanscript();
	}
}
