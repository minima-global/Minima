package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusPrint;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class txpowinfo extends CommandFunction {

	public txpowinfo() {
		super("txpowinfo");
		
		setHelp("[txpowid]", "Return info about a specific TxPOW", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Get the TxPOWID
		String TxPOWID = zInput[1];
		
		//Get the current balance of the user for all tokens..
		Message msg = getResponseMessage(ConsensusPrint.CONSENSUS_TXPOW);
		msg.addString("txpow", TxPOWID);
		
		//Post It..
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new txpowinfo();
	}
}
