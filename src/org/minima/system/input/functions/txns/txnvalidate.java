package org.minima.system.input.functions.txns;

import org.minima.system.brains.ConsensusTxn;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class txnvalidate extends CommandFunction{

	public txnvalidate() {
		super("txnvalidate");
		setHelp("[id]", "Check the validity (params scripts sigs) of the transaction", "");
	}

	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Which transaction
		int txn = Integer.parseInt(zInput[1]);
		
		//Send to the consensus Handler
		Message msg = getResponseMessage(ConsensusTxn.CONSENSUS_TXNVALIDATE);
		msg.addInt("transaction", txn);
	
		getMainHandler().getConsensusHandler().PostMessage(msg);		
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new txnvalidate();
	}
}
