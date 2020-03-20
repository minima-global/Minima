package org.minima.system.input.functions.txns;

import org.minima.system.brains.ConsensusTxn;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class txnstate extends CommandFunction {

	public txnstate() {
		super("txnstate");
		setHelp("[id] [port] [value]", "Add a state variable to the transaction", "");
	}

	@Override
	public void doFunction(String[] zInput) throws Exception {
		int txn 			= Integer.parseInt(zInput[1]);
		
		int txnport 	    = Integer.parseInt(zInput[2]);
		String variable   	= zInput[3];
		
		//Send to the consensus Handler
		Message msg = getResponseMessage(ConsensusTxn.CONSENSUS_TXNSTATEVAR);
		msg.addInt("transaction", txn);
		msg.addInt("stateport", txnport);
		msg.addString("statevariable", variable);
		
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new txnstate();
	}
}
