package org.minima.system.input.functions.txns;

import org.minima.system.brains.ConsensusTxn;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class txndelete extends CommandFunction{

	public txndelete() {
		super("txndelete");
		setHelp("[id]", "Delete the custom transaction", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Which transaction
		int txn = Integer.parseInt(zInput[1]);
		
		//Send to the consensus Handler
		//Message msg = new Message(ConsensusTxn.CONSENSUS_TXNDELETE);
		Message msg = getResponseMessage(ConsensusTxn.CONSENSUS_TXNDELETE);
		msg.addInt("transaction", txn);
	
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new txndelete();
	}
}
