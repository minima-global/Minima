package org.minima.system.input.functions;

import org.minima.GlobalParams;
import org.minima.objects.keys.MultiKey;
import org.minima.system.brains.ConsensusPrint;
import org.minima.system.brains.ConsensusUser;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class keys extends CommandFunction {

	public keys() {
		super("keys");
		
		setHelp("(new) (bitlength keys levels)", "List all your public keys or create a new one.", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		if(zInput.length>1) {
			//Create a new Key..
			Message newkey  = getResponseMessage(ConsensusUser.CONSENSUS_NEWKEY);
			
			//Default 
			newkey.addInteger("keys", MultiKey.DEFAULT_KEYS_PER_LEVEL.getAsInt());
			newkey.addInteger("levels", MultiKey.DEFAULT_LEVELS.getAsInt());
			
			if(zInput.length>4) {
				//Get the bit length
				int bitl = Integer.parseInt(zInput[2]);
				newkey.addInteger("bitlength", bitl);
				
				int keys = Integer.parseInt(zInput[3]);
				newkey.addInteger("keys", keys);
				
				int levels = Integer.parseInt(zInput[4]);
				newkey.addInteger("levels", levels);
				
			}else if(zInput.length>2) {
				//Get the bit length
				int bitl = Integer.parseInt(zInput[2]);
				newkey.addInteger("bitlength", bitl);
				
			}else {
				newkey.addInteger("bitlength", GlobalParams.MINIMA_DEFAULT_HASH_STRENGTH);
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
