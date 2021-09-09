package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusNet;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class resync extends CommandFunction{

	public resync() {
		super("resync");
		setHelp("", "Send full Greeting resync message to all peers", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Get a response message
		Message msg = getResponseMessage(ConsensusNet.CONSENSUS_NET_FULLTREERESYSNC);
		
		//Send a backup message - with no request to shutdown at the end..
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new resync();
	}

}
