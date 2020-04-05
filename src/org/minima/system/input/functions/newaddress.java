package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusUser;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class newaddress extends CommandFunction{

	public newaddress() {
		super("newaddress");
		setHelp("(hashbits)", "Create a new address to receive funds", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Get a response message
		Message msg = getResponseMessage(ConsensusUser.CONSENSUS_NEWSIMPLE);
				
		if(zInput.length>1) {
			int bits = Integer.parseInt(zInput[1]);
			msg.addInt("bitlength", bits);
		}
		
		//Post a new Message
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new newaddress();
	}
}
