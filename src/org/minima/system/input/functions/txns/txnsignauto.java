package org.minima.system.input.functions.txns;

import org.minima.system.brains.ConsensusTxn;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class txnsignauto extends CommandFunction {

	public txnsignauto() {
		super("txnsignauto");
		setHelp("[id]", "Clear all previous signatures. Scan inputs and auto sign simple addresses. Useful if you edit a txnauto.. you can resign with this.", "");
	}

	@Override
	public void doFunction(String[] zInput) throws Exception {
		int txn 			= Integer.parseInt(zInput[1]);
		
		//Send to the consensus Handler
		Message msg = getResponseMessage(ConsensusTxn.CONSENSUS_TXNAUTOSIGN);
		msg.addInt("transaction", txn);
		
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new txnsignauto();
	}
}
