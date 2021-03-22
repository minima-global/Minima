package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusUser;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class currentaddress extends CommandFunction {

	public currentaddress() {
		super("currentaddress");
		
		setHelp("", "Return your current change address and key info.. used internally", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		Message msg = getResponseMessage(ConsensusUser.CONSENSUS_CURRENTADDRESS);
			
//		check for new and return other details..
		
		//Post It..
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new currentaddress();
	}
}
