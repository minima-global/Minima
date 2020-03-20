package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusPrint;
import org.minima.system.brains.ConsensusUser;
import org.minima.system.input.CommandFunction;

public class keys extends CommandFunction {

	public keys() {
		super("keys");
		
		setHelp("(new)", "Create a new key pair or return a list of all the addresses and public keys in this account", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		if(zInput.length>1) {
			//Create a new Key..
			getMainHandler().getConsensusHandler().PostMessage(getResponseMessage(ConsensusUser.CONSENSUS_NEWKEY));
			return;
		}
			
		//Post It..
		getMainHandler().getConsensusHandler().PostMessage(getResponseMessage(ConsensusPrint.CONSENSUS_KEYS));
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new keys();
	}
}
