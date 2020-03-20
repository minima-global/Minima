package org.minima.system.input.functions.transfer;

import org.minima.objects.base.MiniData;
import org.minima.system.brains.ConsensusUser;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class exportkey extends CommandFunction{

	public exportkey() {
		super("exportkey");
		
		setHelp("[public key]", "Export the private key seed of the public key", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//The PubKey
		String pubks = zInput[1];
		
		//Connect to a specific host:port
		Message export  = new Message(ConsensusUser.CONSENSUS_EXPORTKEY).addObject("publickey", new MiniData(pubks));
		getMainHandler().getConsensusHandler().PostMessage(export);
		
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new exportkey();
	}
}
