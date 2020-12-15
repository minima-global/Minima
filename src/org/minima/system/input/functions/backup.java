package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusBackup;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class backup extends CommandFunction{

	public backup() {
		super("backup");
		setHelp("(file)", "Backup the current details. If file specified saves to the file so you can restore.", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		if(zInput.length>1) {
			//Get the file..
			String file = zInput[1];
			
			//Get a response message
			Message msg = getResponseMessage(ConsensusBackup.CONSENSUSBACKUP_SYNCBACKUP);
			msg.addString("file", file);
			
			//Send a backup message - with no request to shutdown at the end..
			getMainHandler().getConsensusHandler().PostMessage(msg);

			return;
		}
		
		
		//Get a response message
		Message msg = getResponseMessage(ConsensusBackup.CONSENSUSBACKUP_BACKUP);
		
		//Send a backup message - with no request to shutdown at the end..
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new backup();
	}

}
