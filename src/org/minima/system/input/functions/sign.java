package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusUser;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class sign extends CommandFunction{

	public sign() {
		super("sign");
		
		setHelp("[data] [public key]", "Sign arbitrary data with one of your keys", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Get the data - in 0x format
		String data = zInput[1];
		String pubk = zInput[2];
		
		//Send to the consensus Handler
		Message msg = getResponseMessage(ConsensusUser.CONSENSUS_SIGN);
		msg.addString("data", data);
		msg.addString("publickey", pubk);
		
		getMainHandler().getConsensusHandler().PostMessage(msg);		
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new sign();
	}
}
