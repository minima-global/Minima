package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusUser;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class consolidate extends CommandFunction {

	public consolidate() {
		super("consolidate");
		
		setHelp("(info|on|off)", "Merge your coins into fewer larger amounts. Turn auto consolidate ON or OFF. Default is ON.", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Get the current balance of the user for all tokens..
		Message msg = getResponseMessage(ConsensusUser.CONSENSUS_CONSOLIDATE);
		
		//Are there any parameters..
		if(zInput.length > 1) {
			String param = zInput[1];
			msg.addString("param", param);
		}
		
		//Post It..
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new consolidate();
	}
}
