package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusPrint;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class coins extends CommandFunction {

	public coins() {
		super("coins");
		setHelp("(address) (spent|unspent|all)", "Return all your tracked coins ( can specify a specific address ) and spent type. Defaults to unspent.", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Get the current balance of the user for all tokens..
		Message msg = getResponseMessage(ConsensusPrint.CONSENSUS_COINS);
				
		if(zInput.length>1) {
			if(zInput.length>2) {
				//Order matters
				msg.addString("address", zInput[1]);
				msg.addString("type", zInput[2]);
			}else {
				if(zInput[1].startsWith("0x") || zInput[1].startsWith("Mx")) {
					//It's an address
					msg.addString("address", zInput[1]);
				}else {
					//It's a spend type
					msg.addString("type", zInput[1]);
				}
			}
		}
			
		//Post It..
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new coins();
	}
}