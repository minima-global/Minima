package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusUser;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class verify extends CommandFunction{

	public verify() {
		super("verify");
		
		setHelp("[data] [public key] [signature]", "Verify data signature", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Get the data - in 0x format
		String data = zInput[1];
		String pubk = zInput[2];
		String sig  = zInput[3];
		
		//Send to the consensus Handler
		Message msg = getResponseMessage(ConsensusUser.CONSENSUS_VERIFY);
		msg.addString("data", data);
		msg.addString("publickey", pubk);
		msg.addString("signature", sig);
		
		getMainHandler().getConsensusHandler().PostMessage(msg);		
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new verify();
	}
}
