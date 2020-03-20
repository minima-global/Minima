package org.minima.system.input.functions.transfer;

import org.minima.objects.base.MiniData;
import org.minima.system.brains.ConsensusUser;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class importkey extends CommandFunction{

	public importkey() {
		super("importkey");
		
		setHelp("[private key seed]", "Import the private key seed of the public key", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//The PrivKey
		String privkey = zInput[1];
		
		//Connect to a specific host:port
		Message export  = new Message(ConsensusUser.CONSENSUS_IMPORTKEY).addObject("privatekey", new MiniData(privkey));
		getMainHandler().getConsensusHandler().PostMessage(export);
		
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new importkey();
	}
}
