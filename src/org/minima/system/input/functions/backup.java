package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusBackup;
import org.minima.system.input.CommandFunction;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class backup extends CommandFunction{

	public backup() {
		super("backup");
		setHelp("", "Backup the current User details (done automatically when you quit or your balance changes)", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Get a response message
		Message msg = getResponseMessage(ConsensusBackup.CONSENSUSBACKUP_BACKUP);
		
		//Send a backup message - with no request to shutdown at the end..
		getMainHandler().getConsensusHandler().PostMessage(msg);
		
		getResponseStream().endStatus(true, "");
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new backup();
	}

}
