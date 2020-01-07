package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusPrint;
import org.minima.system.brains.ConsensusUser;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class newscript extends CommandFunction{

	public newscript() {
		super("newscript");
		
		setHelp("[script]", "Create a new Address based on the given script", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Get the script
		String script = zInput[1];
		
		//Send to the consensus Handler
		Message msg = getResponseMessage(ConsensusUser.CONSENSUS_NEWSCRIPT);
		msg.addString("script", script);
	
		getMainHandler().getConsensusHandler().PostMessage(msg);		
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new newscript();
	}
}
