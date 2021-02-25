package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusBackup;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class reset extends CommandFunction{

	public reset() {
		super("reset");
		setHelp("", "RESET the whole system to a blank state.. ( DEBUG FUNCTION )", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Get a response message
		Message msg = getResponseMessage(ConsensusBackup.CONSENSUSBACKUP_RESET);
		
		//Send a backup message - with no request to shutdown at the end..
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new reset();
	}

}
