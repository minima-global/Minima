package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusHandler;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class automine extends CommandFunction{

	public automine() {
		super("automine");
		
		setHelp("[on|off]", 
				"Start mining with full CPU power. BootStrap measure.","Doesn't require you to send a transaction, just mines constantly.");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		String onff = zInput[1];
	
		boolean mining = false;
		if(zInput[1].equals("on")) {
			//Turn auto mining ON
			mining = true;
		}
		
		//Create a blank transaction
		Message mine = getResponseMessage(ConsensusHandler.CONSENSUS_ACTIVATEMINE)
						.addObject("automining", mining);
		
		getMainHandler().getConsensusHandler().PostMessage(mine);
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new automine();
	}
}
