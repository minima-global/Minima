package org.minima.system.input.functions.txns;

import org.minima.system.brains.ConsensusTxn;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class txnpost extends CommandFunction {

	public txnpost() {
		super("txnpost");
		setHelp("[id]", "Post the transaction the the network", "");
	}

	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Which transaction
		int txn = Integer.parseInt(zInput[1]);
		
		//Send to the consensus Handler
		Message msg = getResponseMessage(ConsensusTxn.CONSENSUS_TXNPOST);
		msg.addInt("transaction", txn);
	
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new txnpost();
	}
}
