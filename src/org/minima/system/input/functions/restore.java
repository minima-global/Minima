package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusBackup;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class restore extends CommandFunction{

	public restore() {
		super("restore");
		setHelp("[file]", "Restore all details from a backup", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Get the file..
		String file = zInput[1];
		
		//Get a response message
		Message msg = getResponseMessage(ConsensusBackup.CONSENSUSBACKUP_SYNCRESTORE);
		msg.addString("file", file);
		
		//Send a backup message - with no request to shutdown at the end..
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new restore();
	}

}
