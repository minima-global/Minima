package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusUser;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class extrascript extends CommandFunction{

	public extrascript() {
		super("extrascript");
		
		setHelp("[script]", "Add a new script, but do not keep all coins of that address.", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Get the script
		String script = zInput[1];
		
		//Send to the consensus Handler
		Message msg = getResponseMessage(ConsensusUser.CONSENSUS_EXTRASCRIPT);
		msg.addString("script", script);
	
		getMainHandler().getConsensusHandler().PostMessage(msg);		
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new extrascript();
	}
}
