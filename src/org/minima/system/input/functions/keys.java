package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusPrint;
import org.minima.system.brains.ConsensusUser;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class keys extends CommandFunction {

	public keys() {
		super("keys");
		
		setHelp("(new) (bitlength)", "List all your public keys or create a new one. Default is 256 bits can specify.", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		if(zInput.length>1) {
			//Create a new Key..
			Message newkey  = getResponseMessage(ConsensusUser.CONSENSUS_NEWKEY);
			
			if(zInput.length>2) {
				//Get the bitlength
				int bitl = Integer.parseInt(zInput[2]);
				
				newkey.addInt("bitlength", bitl);
			}else {
				newkey.addInt("bitlength", 256);
			}
			
			getMainHandler().getConsensusHandler().PostMessage(newkey);
			
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
