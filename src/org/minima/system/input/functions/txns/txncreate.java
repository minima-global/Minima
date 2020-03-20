package org.minima.system.input.functions.txns;

import org.minima.system.brains.ConsensusTxn;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class txncreate extends CommandFunction {

	public txncreate() {
		super("txncreate");
		setHelp("", "(id) Create a new custom transaction with either a random or specified ID", "");
	}

	@Override
	public void doFunction(String[] zInput) throws Exception {
		Message msg = getResponseMessage(ConsensusTxn.CONSENSUS_TXNCREATE);
		
		if(zInput.length>1) {
			//Specified the id
			msg.addInt("id", Integer.parseInt(zInput[1]));
		}
		
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new txncreate();
	}
}
